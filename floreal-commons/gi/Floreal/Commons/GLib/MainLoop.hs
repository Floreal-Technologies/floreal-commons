module Floreal.Commons.GLib.MainLoop
  ( postMain
  , postAfter
  , postIdle
  , debounced
  ) where

import Effectful (Limit (Unlimited), Persistence (Persistent), UnliftStrategy (ConcUnlift), withEffToIO)
import Effectful.Log (Log)
import GI.GLib qualified as GLib

import Floreal.Commons.Exception (guardedLog)

-- | Run the action one time on the GLib main loop, when it is next
-- idle. it needs to be unlifted with a persistent concurrent strategy.
postMain :: (IOE :> es, Log :> es) => Eff es () -> Eff es ()
postMain action = later action (postIdle GLib.PRIORITY_DEFAULT)

-- | Run the action one time on the GLib main loop, after the delay in
-- milliseconds.
postAfter :: (IOE :> es, Log :> es) => Word32 -> Eff es () -> Eff es ()
postAfter delay action =
  later action $ \run -> void (GLib.timeoutAdd GLib.PRIORITY_DEFAULT delay (run $> GLib.SOURCE_REMOVE))

-- | Run the action one time on the GLib main loop, at the priority.
-- The action must not throw.
postIdle :: Int32 -> IO () -> IO ()
postIdle priority action = void (GLib.idleAdd priority (action $> GLib.SOURCE_REMOVE))

-- | A runner that keeps at most one action waiting. Each call removes the
-- waiting action, and runs the new one after the delay in milliseconds.
-- The actions must not throw.
debounced :: Word32 -> IO (IO () -> IO ())
debounced delay = do
  waiting <- newIORef Nothing
  pure $ \action -> do
    readIORef waiting >>= traverse_ GLib.sourceRemove
    sourceId <-
      GLib.timeoutAdd GLib.PRIORITY_DEFAULT delay $ do
        writeIORef waiting Nothing
        action $> GLib.SOURCE_REMOVE
    writeIORef waiting (Just sourceId)

later :: (IOE :> es, Log :> es) => Eff es () -> (IO () -> IO ()) -> Eff es ()
later action register =
  withEffToIO (ConcUnlift Persistent Unlimited) $ \unlift -> register (unlift (guardedLog "main loop" action))
