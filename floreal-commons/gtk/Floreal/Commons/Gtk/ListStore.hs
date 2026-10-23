{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RoleAnnotations #-}

-- | A 'Gio.ListStore' of Haskell values.
--
-- Values must be plain data: an item keeps its value alive, and evaluates
-- it to weak head normal form only.
module Floreal.Commons.Gtk.ListStore
  ( ListStore
  , newListStore
  , listModel
  , appendItem
  , spliceItems
  , replaceItems
  , getItem
  , itemValue
  , bindItems
  , recycleItems
  ) where

import Control.Exception (evaluate)
import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
import Data.GI.Base (GObject, castTo, glibType, new)
import Data.GI.Base.GObject (gobjectGetUserData, gobjectSetUserData)
import Data.GI.Base.GQuark (GQuark, gQuarkFromString)
import Data.GI.Base.ManagedPtr (newObject, withManagedPtr)
import Foreign.Ptr (Ptr, castPtr)
import GI.GObject qualified as GObject
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import System.IO.Unsafe (unsafePerformIO)

import Floreal.Commons.Gtk.Signal (onSelf)

-- | Each item is a plain 'GObject.Object' that holds a 'Dynamic'.
newtype ListStore a = ListStore Gio.ListStore

type role ListStore nominal

newListStore :: IO (ListStore a)
newListStore = ListStore <$> (glibType @GObject.Object >>= Gio.listStoreNew)

-- | A new reference to the store as a 'Gio.ListModel'. Functions that take
-- ownership, such as 'Gtk.singleSelectionNew', can have it.
listModel :: ListStore a -> IO Gio.ListModel
listModel (ListStore store) =
  withManagedPtr store (\pointer -> newObject Gio.ListModel (castPtr pointer :: Ptr Gio.ListModel))

appendItem :: (Typeable a) => ListStore a -> a -> IO ()
appendItem (ListStore store) value = box value >>= Gio.listStoreAppend store

-- | Replace @removed@ items at @position@ with the values, in one
-- @items-changed@. @position + removed@ must not exceed the item count.
spliceItems :: (Typeable a) => ListStore a -> Word32 -> Word32 -> List a -> IO ()
spliceItems (ListStore store) position removed values = do
  items <- traverse box values
  Gio.listStoreSplice store position removed items

-- | Replace all items, in one @items-changed@.
replaceItems :: (Typeable a) => ListStore a -> List a -> IO ()
replaceItems (ListStore store) values = do
  count <- Gio.listModelGetNItems store
  spliceItems (ListStore store) 0 count values

getItem :: (Typeable a) => ListStore a -> Word32 -> IO (Maybe a)
getItem (ListStore store) position =
  Gio.listModelGetItem store position >>= fmap join . traverse itemValue

-- | 'Nothing' if the item holds no value of type @a@.
itemValue :: (Typeable a, GObject o) => o -> IO (Maybe a)
itemValue item = (>>= fromDynamic) <$> gobjectGetUserData item valueKey

-- | Connect the @#bind@ handler of a factory. The action gets the item and
-- its value, and returns its cleanup. The cleanup runs at @#unbind@,
-- @#teardown@ and the next @#bind@.
bindItems :: (Typeable a) => Gtk.SignalListItemFactory -> (Gtk.ListItem -> a -> IO (IO ())) -> IO ()
bindItems factory setup = do
  void $ onSelf factory #bind $ \object -> withListItem object $ \listItem -> do
    slot <- cleanupSlot listItem
    release slot
    value <- Gtk.listItemGetItem listItem >>= fmap join . traverse itemValue
    forM_ value (setup listItem >=> writeIORef slot)
  void $ onSelf factory #unbind releaseItem
  void $ onSelf factory #teardown releaseItem
  where
    withListItem object action = castTo Gtk.ListItem object >>= traverse_ action
    releaseItem object = withListItem object $ \listItem ->
      gobjectGetUserData listItem cleanupKey >>= traverse_ release
    -- Resetting drops the cleanup, which can hold the item.
    release slot = do
      cleanup <- readIORef slot
      writeIORef slot (pure ())
      cleanup

-- | Connect the @#setup@ and @#bind@ handlers of a factory. @#setup@
-- builds one row for each list item. @#bind@ gives that row and the value
-- to the action, which returns its cleanup, as in 'bindItems'.
recycleItems
  :: (Typeable a, Typeable r, Gtk.IsWidget w)
  => Gtk.SignalListItemFactory
  -> IO (w, r)
  -> (r -> a -> IO (IO ()))
  -> IO ()
recycleItems factory build setup = do
  void $ onSelf factory #setup $ \object -> castTo Gtk.ListItem object >>= traverse_ create
  bindItems factory $ \listItem value ->
    gobjectGetUserData listItem rowKey <&> (>>= fromDynamic) >>= \case
      Nothing -> pure (pure ())
      Just row -> setup row value
  where
    create listItem = do
      (child, row) <- build
      Gtk.listItemSetChild listItem (Just child)
      gobjectSetUserData listItem rowKey (toDyn row)

-- | Made at the first bind.
cleanupSlot :: Gtk.ListItem -> IO (IORef (IO ()))
cleanupSlot listItem =
  gobjectGetUserData listItem cleanupKey >>= \case
    Just slot -> pure slot
    Nothing -> do
      slot <- newIORef (pure ())
      gobjectSetUserData listItem cleanupKey slot
      pure slot

box :: (Typeable a) => a -> IO GObject.Object
box value = do
  evaluated <- evaluate value
  item <- new GObject.Object []
  gobjectSetUserData item valueKey (toDyn evaluated)
  pure item

valueKey :: GQuark Dynamic
valueKey = unsafePerformIO (gQuarkFromString "floreal-commons-list-store-value")
{-# NOINLINE valueKey #-}

rowKey :: GQuark Dynamic
rowKey = unsafePerformIO (gQuarkFromString "floreal-commons-list-item-row")
{-# NOINLINE rowKey #-}

cleanupKey :: GQuark (IORef (IO ()))
cleanupKey = unsafePerformIO (gQuarkFromString "floreal-commons-list-item-cleanup")
{-# NOINLINE cleanupKey #-}
