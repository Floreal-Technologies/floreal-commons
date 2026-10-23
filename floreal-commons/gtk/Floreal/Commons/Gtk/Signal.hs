{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Signal handlers and repeating sources that cannot keep their owner alive.
module Floreal.Commons.Gtk.Signal
  ( Guarded
  , onSelf
  , onBehalf
  , everyOnBehalf
  ) where

import Data.Coerce (coerce)
import Data.GI.Base (on)
import Data.GI.Base.ManagedPtr (withManagedPtr, wrapObject)
import Data.GI.Base.Signals (SignalHandlerId, SignalInfo (HaskellCallbackType), SignalProxy, disconnectSignalHandler)
import Foreign.Concurrent qualified as Concurrent
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (sizeOf)
import GI.GLib qualified as GLib
import GI.GObject qualified as GObject
import GI.Gtk qualified as Gtk

-- | Callbacks that can be skipped when their owner is gone.
class Guarded f where
  guarded :: IO (Maybe a) -> (a -> f) -> f

instance Guarded (IO ()) where
  guarded fetch callback = fetch >>= traverse_ callback

instance Guarded (IO Bool) where
  guarded fetch callback = fetch >>= maybe (pure False) callback

instance (Guarded f) => Guarded (x -> f) where
  guarded fetch callback x = guarded fetch (\owner -> callback owner x)

-- | Connect a handler that lives as long as its emitter.
-- The handler must reach the emitter through @?self@, not by capturing it.
onSelf
  :: (GObject.GObject emitter, SignalInfo info)
  => emitter
  -> SignalProxy emitter info
  -> ((?self :: emitter) => HaskellCallbackType info)
  -> IO SignalHandlerId
onSelf emitter signal handler = on emitter signal handler

-- | Connect a handler to an emitter that outlives the owner.
-- The handler holds the owner weakly and receives it as its first argument.
-- It is disconnected when the owner is destroyed.
onBehalf
  :: (Gtk.IsWidget owner, GObject.IsObject emitter, SignalInfo info, Guarded (HaskellCallbackType info))
  => owner
  -> emitter
  -> SignalProxy emitter info
  -> (owner -> HaskellCallbackType info)
  -> IO ()
onBehalf owner emitter signal handler = do
  weak <- newWeak owner
  handlerId <- on emitter signal (guarded (upgrade weak) handler)
  whenDestroyed owner $ do
    connected <- GObject.signalHandlerIsConnected emitter handlerId
    when connected (disconnectSignalHandler emitter handlerId)

-- | Run the action every interval in milliseconds, as long as the owner is alive.
-- The action holds the owner weakly and receives it as its argument.
everyOnBehalf
  :: (Gtk.IsWidget owner)
  => owner
  -> Word32
  -> (owner -> IO ())
  -> IO ()
everyOnBehalf owner interval action = do
  weak <- newWeak owner
  sourceId <-
    GLib.timeoutAdd GLib.PRIORITY_DEFAULT interval $
      upgrade weak >>= \case
        Nothing -> pure GLib.SOURCE_REMOVE
        Just alive -> action alive $> GLib.SOURCE_CONTINUE
  whenDestroyed owner (void (GLib.sourceRemove sourceId))

whenDestroyed :: (Gtk.IsWidget owner) => owner -> IO () -> IO ()
whenDestroyed owner action = do
  widget <- Gtk.toWidget owner
  void $ on widget #destroy action

newtype Weak a = Weak (ForeignPtr GObject.WeakRef)

newWeak :: (GObject.GObject a) => a -> IO (Weak a)
newWeak object = do
  slot <- mallocBytes (sizeOf nullPtr)
  withManagedPtr object (g_weak_ref_init slot . castPtr)
  Weak <$> Concurrent.newForeignPtr slot (g_weak_ref_clear slot >> free slot)

upgrade :: (GObject.GObject a) => Weak a -> IO (Maybe a)
upgrade (Weak slot) = do
  pointer <- withForeignPtr slot g_weak_ref_get
  if pointer == nullPtr
    then pure Nothing
    else Just <$> wrapObject coerce pointer

foreign import ccall unsafe "g_weak_ref_init"
  g_weak_ref_init :: Ptr GObject.WeakRef -> Ptr GObject.Object -> IO ()

foreign import ccall unsafe "g_weak_ref_get"
  g_weak_ref_get :: Ptr GObject.WeakRef -> IO (Ptr GObject.Object)

foreign import ccall unsafe "g_weak_ref_clear"
  g_weak_ref_clear :: Ptr GObject.WeakRef -> IO ()
