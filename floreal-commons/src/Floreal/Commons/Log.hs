module Floreal.Commons.Log
  ( withStderrLogger
  , runStderrLog
  ) where

import Data.Text.IO qualified as Text
import Effectful (withRunInIO)
import Effectful.Log (Log, LogMessage, Logger, defaultLogLevel, mkLogger, runLog, showLogMessage, shutdownLogger, waitForLogger)
import System.IO (stderr)

withLogger :: (LogMessage -> IO ()) -> (Logger -> IO a) -> IO a
withLogger sink use = do
  logger <- mkLogger "floreal" sink
  use logger `finally` (waitForLogger logger >> shutdownLogger logger)

withStderrLogger :: (Logger -> IO a) -> IO a
withStderrLogger = withLogger (Text.hPutStrLn stderr . showLogMessage Nothing)

-- | Run the Log effect with a stderr logger, for the named component.
runStderrLog :: (IOE :> es) => Text -> Eff (Log : es) a -> Eff es a
runStderrLog component action =
  withRunInIO $ \run -> withStderrLogger $ \logger -> run (runLog component logger defaultLogLevel action)
