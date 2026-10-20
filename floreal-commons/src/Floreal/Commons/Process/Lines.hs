module Floreal.Commons.Process.Lines
  ( Line (..)
  , lineReader
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Effectful.Exception qualified as E

import Floreal.Commons.Process (Child, Output, Process, readChunk)

data Line = Line ByteString | TooLong | Closed

lineReader :: (Process :> es, IOE :> es) => Int -> Child -> Output -> Eff es (Eff es Line)
lineReader maxLine child output = do
  carry <- liftIO (newIORef BS.empty)
  finished <- liftIO (newIORef False)
  let next =
        liftIO (readIORef finished) >>= \case
          True -> pure Closed
          False -> liftIO (readIORef carry) >>= \rest -> scan [rest] (BS.length rest) rest
      scan chunks size latest = case BS8.elemIndex '\n' latest of
        Just i
          | size - BS.length latest + i > maxLine -> pure TooLong
          | otherwise -> do
              liftIO (writeIORef carry (BS.drop (i + 1) latest))
              pure (Line (BS.concat (reverse (BS.take i latest : drop 1 chunks))))
        Nothing
          | size > maxLine -> pure TooLong
          | otherwise ->
              E.try @IOException (readChunk child output 65_536) >>= \case
                Right chunk | not (BS.null chunk) -> scan (chunk : chunks) (size + BS.length chunk) chunk
                _ -> do
                  liftIO (writeIORef finished True)
                  liftIO (writeIORef carry BS.empty)
                  let rest = BS.concat (reverse chunks)
                  pure (if BS.null rest then Closed else Line rest)
  pure next
