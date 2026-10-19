module Floreal.Commons.Exception
  ( guarded
  , guardedLog
  ) where

import Control.Exception (SomeAsyncException, fromException)
import Data.Text qualified as Text
import Effectful.Exception (trySync)
import Effectful.Log (Log, logAttention_)

guarded :: IO a -> IO (Either SomeException a)
guarded action =
  try action >>= \case
    Left e
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> pure (Left e)
    Right a -> pure (Right a)

guardedLog :: (Log :> es) => Text -> Eff es () -> Eff es ()
guardedLog what action =
  trySync action >>= \case
    Left e -> void (trySync (logAttention_ (what <> ": " <> Text.show e)))
    Right () -> pure ()
