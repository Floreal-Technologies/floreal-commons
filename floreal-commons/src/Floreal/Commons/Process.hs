{-# LANGUAGE TypeFamilies #-}

module Floreal.Commons.Process
  ( Stdio (..)
  , Detach (..)
  , Spawn (..)
  , spawnOf
  , Output (..)
  , Reach (..)
  , Child (..)
  , Process (..)
  , spawn
  , processId
  , writeInput
  , closeInput
  , readChunk
  , exitCodeOf
  , waitExit
  , terminate
  , kill
  , release
  , runProcessIO
  ) where

import Control.Concurrent (ThreadId, forkIO, forkIOWithUnmask, killThread)
import Control.Concurrent.STM (STM, TVar, atomically, check, modifyTVar', newEmptyTMVarIO, newTVarIO, putTMVar, readTMVar, readTVar, readTVarIO, retry, stateTVar, writeTVar)
import Control.Exception (mask_, onException)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Effectful (Dispatch (Dynamic), DispatchOf)
import Effectful.Dispatch.Dynamic (interpret_, send)
import System.Exit (ExitCode)
import System.IO (Handle, IOMode (ReadMode, WriteMode), hClose, hFlush, hSetBinaryMode, openFile)
import System.OsPath qualified as OsPath
import System.Process qualified as P

import Floreal.Commons.Process.Native (Group, groupOf, killChild, killGroup, nullDevice, terminateGroup)

data Stdio = Pipe | Inherit | NoStream | NullDevice
  deriving stock (Eq, Show)

data Detach = SameGroup | NewGroup | NewSession
  deriving stock (Eq, Show)

data Spawn = Spawn
  { program :: OsPath
  , arguments :: List String
  , folder :: Maybe OsPath
  , environment :: Maybe (List (String, String))
  , stdin :: Stdio
  , stdout :: Stdio
  , stderr :: Stdio
  , detach :: Detach
  , closeFds :: Bool
  }
  deriving stock (Eq, Show)

-- |
-- >>> import System.OsPath (osp)
-- >>> spawnOf [osp|wpctl|] ["status"]
-- Spawn {program = "wpctl", arguments = ["status"], folder = Nothing, environment = Nothing, stdin = Inherit, stdout = Inherit, stderr = Inherit, detach = SameGroup, closeFds = False}
spawnOf :: OsPath -> List String -> Spawn
spawnOf path args =
  Spawn
    { program = path
    , arguments = args
    , folder = Nothing
    , environment = Nothing
    , stdin = Inherit
    , stdout = Inherit
    , stderr = Inherit
    , detach = SameGroup
    , closeFds = False
    }

data Output = StandardOutput | StandardError
  deriving stock (Eq, Show)

data Reach = OnlyChild | WholeGroup
  deriving stock (Eq, Show)

newtype Child = Child Int
  deriving stock (Eq, Ord, Show)

data Process :: Effect where
  SpawnChild :: Spawn -> Process m Child
  ProcessId :: Child -> Process m (Maybe Int)
  WriteInput :: Child -> ByteString -> Process m ()
  CloseInput :: Child -> Process m ()
  ReadChunk :: Child -> Output -> Int -> Process m ByteString
  ExitCodeOf :: Child -> Process m (Maybe ExitCode)
  WaitExit :: Child -> Process m ExitCode
  Terminate :: Reach -> Child -> Process m ()
  Kill :: Reach -> Child -> Process m ()
  Release :: Child -> Process m ()

type instance DispatchOf Process = Dynamic

spawn :: (Process :> es) => Spawn -> Eff es Child
spawn s = send (SpawnChild s)

processId :: (Process :> es) => Child -> Eff es (Maybe Int)
processId child = send (ProcessId child)

writeInput :: (Process :> es) => Child -> ByteString -> Eff es ()
writeInput child bytes = send (WriteInput child bytes)

closeInput :: (Process :> es) => Child -> Eff es ()
closeInput child = send (CloseInput child)

readChunk :: (Process :> es) => Child -> Output -> Int -> Eff es ByteString
readChunk child output size = send (ReadChunk child output size)

exitCodeOf :: (Process :> es) => Child -> Eff es (Maybe ExitCode)
exitCodeOf child = send (ExitCodeOf child)

waitExit :: (Process :> es) => Child -> Eff es ExitCode
waitExit child = send (WaitExit child)

terminate :: (Process :> es) => Reach -> Child -> Eff es ()
terminate reach child = send (Terminate reach child)

kill :: (Process :> es) => Reach -> Child -> Eff es ()
kill reach child = send (Kill reach child)

release :: (Process :> es) => Child -> Eff es ()
release child = send (Release child)

data Entry = Entry
  { process :: P.ProcessHandle
  , group :: Group
  , inputPipe :: Maybe Handle
  , outputStream :: Maybe Stream
  , errorStream :: Maybe Stream
  , released :: TVar Bool
  }

data Stream = Stream
  { pipe :: Handle
  , pump :: ThreadId
  , buffer :: Buffer
  }

data Buffer = Buffer
  { chunks :: TVar (Seq ByteString)
  , buffered :: TVar Int
  , ended :: TVar Bool
  }

runProcessIO :: (IOE :> es) => Eff (Process : es) a -> Eff es a
runProcessIO action = do
  table <- liftIO (newTVarIO Map.empty)
  counter <- liftIO (newTVarIO 0)
  interpret_ (liftIO . run table counter) action

run :: TVar (Map Int Entry) -> TVar Int -> Process m a -> IO a
run table counter = \case
  SpawnChild s -> do
    path <- OsPath.decodeFS (program s)
    dir <- traverse OsPath.decodeFS (folder s)
    mask_ $ do
      input <- stream ReadMode (stdin s)
      output <- stream WriteMode (stdout s) `onException` closeNull [input]
      errors <- stream WriteMode (stderr s) `onException` closeNull [input, output]
      let config =
            (P.proc path (arguments s))
              { P.cwd = dir
              , P.env = environment s
              , P.std_in = input
              , P.std_out = output
              , P.std_err = errors
              , P.close_fds = closeFds s
              }
      created@(i, o, e, handle) <-
        P.createProcess (detached (detach s) config) `onException` closeNull [input, output, errors]
      flip onException (P.cleanupProcess created) $ do
        forM_ (catMaybes [i, o, e]) (\h -> hSetBinaryMode h True)
        g <- groupOf handle
        flag <- newTVarIO False
        out <- traverse (startStream flag) o
        err <- traverse (startStream flag) e
        atomically $ do
          n <- stateTVar counter (\n -> (n, n + 1))
          modifyTVar' table (Map.insert n (Entry handle g i out err flag))
          pure (Child n)
  ProcessId child -> entryOf child >>= \entry -> fmap fromIntegral <$> P.getPid (process entry)
  WriteInput child bytes -> entryOf child >>= \entry -> piped (inputPipe entry) >>= \h -> BS.hPut h bytes >> hFlush h
  CloseInput child -> entryOf child >>= \entry -> piped (inputPipe entry) >>= hClose
  ReadChunk child output size -> entryOf child >>= \entry -> piped (streamOf output entry) >>= atomically . takeChunk size (released entry) . buffer
  ExitCodeOf child -> entryOf child >>= P.getProcessExitCode . process
  WaitExit child -> entryOf child >>= waitApart . process
  Terminate OnlyChild child -> entryOf child >>= P.terminateProcess . process
  Terminate WholeGroup child -> entryOf child >>= \entry -> terminateGroup (process entry) (group entry)
  Kill OnlyChild child -> entryOf child >>= killChild . process
  Kill WholeGroup child -> entryOf child >>= \entry -> killGroup (process entry) (group entry)
  Release (Child n) ->
    mask_ $
      atomically (forget n)
        >>= maybe unknown (void . forkIO . closeAll)
  where
    entryOf (Child n) = readTVarIO table >>= maybe unknown pure . Map.lookup n
    unknown = ioError (userError "unknown child")
    piped = maybe (ioError (userError "not a pipe")) pure
    forget n =
      readTVar table >>= \entries ->
        traverse
          (\entry -> writeTVar (released entry) True >> writeTVar table (Map.delete n entries) $> entry)
          (Map.lookup n entries)

startStream :: TVar Bool -> Handle -> IO Stream
startStream flag h = do
  b <- Buffer <$> newTVarIO Seq.empty <*> newTVarIO 0 <*> newTVarIO False
  thread <- forkIOWithUnmask (\unmask -> unmask (pumpInto flag h b))
  pure (Stream h thread b)

bufferLimit :: Int
bufferLimit = 1_048_576

pumpInto :: TVar Bool -> Handle -> Buffer -> IO ()
pumpInto flag h b = do
  open <- atomically $ do
    stop <- readTVar flag
    size <- readTVar (buffered b)
    check (stop || size < bufferLimit)
    pure (not stop)
  when open $
    try @IOException (BS.hGetSome h 65_536) >>= \case
      Right chunk
        | not (BS.null chunk) -> do
            atomically $ do
              modifyTVar' (chunks b) (Seq.|> chunk)
              modifyTVar' (buffered b) (+ BS.length chunk)
            pumpInto flag h b
      _ -> atomically (writeTVar (ended b) True)

takeChunk :: Int -> TVar Bool -> Buffer -> STM ByteString
takeChunk size flag b =
  readTVar (chunks b) >>= \case
    chunk Seq.:<| rest
      | BS.length chunk <= size -> taken chunk rest
      | otherwise -> taken (BS.take size chunk) (BS.drop size chunk Seq.:<| rest)
    Seq.Empty -> do
      finished <- (||) <$> readTVar (ended b) <*> readTVar flag
      if finished then pure BS.empty else retry
  where
    taken bytes rest = do
      writeTVar (chunks b) rest
      modifyTVar' (buffered b) (subtract (BS.length bytes))
      pure bytes

closeAll :: Entry -> IO ()
closeAll entry = do
  forM_ (inputPipe entry) (try @IOException . hClose)
  forM_ streams (killThread . pump)
  forM_ streams (try @IOException . hClose . pipe)
  where
    streams = catMaybes [outputStream entry, errorStream entry]

streamOf :: Output -> Entry -> Maybe Stream
streamOf StandardOutput = outputStream
streamOf StandardError = errorStream

stream :: IOMode -> Stdio -> IO P.StdStream
stream mode = \case
  Pipe -> pure P.CreatePipe
  Inherit -> pure P.Inherit
  NoStream -> pure P.NoStream
  NullDevice -> P.UseHandle <$> openFile nullDevice mode

closeNull :: List P.StdStream -> IO ()
closeNull = traverse_ $ \case
  P.UseHandle h -> hClose h
  _ -> pure ()

waitApart :: P.ProcessHandle -> IO ExitCode
waitApart handle = do
  result <- newEmptyTMVarIO
  mask_ (void (forkIO (try @SomeException (P.waitForProcess handle) >>= atomically . putTMVar result)))
  atomically (readTMVar result) >>= either throwIO pure

detached :: Detach -> P.CreateProcess -> P.CreateProcess
detached SameGroup config = config
detached NewGroup config = config {P.create_group = True, P.use_process_jobs = True}
detached NewSession config = config {P.new_session = True}
