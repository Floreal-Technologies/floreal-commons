module Floreal.Commons.Process.Native
  ( Group
  , groupOf
  , terminateGroup
  , killGroup
  , killChild
  , nullDevice
  ) where

import System.Posix.Signals (Signal, sigKILL, sigTERM, signalProcess, signalProcessGroup)
import System.Posix.Types (CPid)
import System.Process (ProcessHandle, getPid)
import System.Process.Internals (ProcessHandle__ (OpenHandle), withProcessHandle)

newtype Group = Group (Maybe CPid)

groupOf :: ProcessHandle -> IO Group
groupOf handle = Group <$> getPid handle

terminateGroup :: ProcessHandle -> Group -> IO ()
terminateGroup _ group = signal sigTERM group

killGroup :: ProcessHandle -> Group -> IO ()
killGroup _ group = signal sigKILL group

killChild :: ProcessHandle -> IO ()
killChild handle =
  withProcessHandle handle $ \case
    OpenHandle pid -> signalProcess sigKILL pid
    _ -> pure ()

signal :: Signal -> Group -> IO ()
signal sig (Group leader) = forM_ leader (try @IOException . signalProcessGroup sig)

nullDevice :: FilePath
nullDevice = "/dev/null"
