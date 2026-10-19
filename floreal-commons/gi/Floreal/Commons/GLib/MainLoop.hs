module Floreal.Commons.GLib.MainLoop
  ( postMain
  , postAfter
  ) where

import Effectful (Limit (Unlimited), Persistence (Persistent), UnliftStrategy (ConcUnlift), withEffToIO)
import Effectful.Log (Log)
import GI.GLib qualified as GLib

import Floreal.Commons.Exception (guardedLog)

-- | Run the action one time on the GLib main loop, when it is next
-- idle. it needs to be unlifted with a persistent concurrent strategy.
postMain :: (IOE :> es, Log :> es) => Eff es () -> Eff es ()
postMain action =
  later action $ \run -> void (GLib.idleAdd GLib.PRIORITY_DEFAULT (run $> GLib.SOURCE_REMOVE))

-- | Run the action one time on the GLib main loop, after the delay in
-- milliseconds.
postAfter :: (IOE :> es, Log :> es) => Word32 -> Eff es () -> Eff es ()
postAfter delay action =
  later action $ \run -> void (GLib.timeoutAdd GLib.PRIORITY_DEFAULT delay (run $> GLib.SOURCE_REMOVE))

later :: (IOE :> es, Log :> es) => Eff es () -> (IO () -> IO ()) -> Eff es ()
later action register =
  withEffToIO (ConcUnlift Persistent Unlimited) $ \unlift -> register (unlift (guardedLog "main loop" action))
