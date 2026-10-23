{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module Floreal.Commons.Gtk.Bind where

import Control.Exception (bracket_)
import Data.GI.Base (SignalProxy (PropertySet))
import Data.GI.Base.Signals (SignalHandlerId, SignalInfo (HaskellCallbackType))
import GI.Adw qualified as Adw
import GI.GObject qualified as GObject
import GI.Gtk qualified as Gtk

import Floreal.Commons.Gtk.Common (paintEditable)
import Floreal.Commons.Gtk.Signal (onSelf)

-- | A widget seen as a value of type `a`.
--
-- * 'paint' shows a value.
-- * 'connect' reports each user change as an update.
-- * 'current' is what 'bind' compares with. After `paint v` it must give
--   `v`.
data Control a = Control
  { paint :: a -> IO ()
  , connect :: ((a -> a) -> IO ()) -> IO Connection
  , current :: IO a
  }

-- | Handlers that 'quietly' blocks. '<>' blocks in order and unblocks in
-- reverse.
data Connection = Connection
  { block :: IO ()
  , unblock :: IO ()
  }

instance Semigroup Connection where
  outer <> inner =
    Connection
      { block = outer.block >> inner.block
      , unblock = inner.unblock >> outer.unblock
      }

instance Monoid Connection where
  mempty = Connection {block = pure (), unblock = pure ()}

connected :: (GObject.IsObject o) => o -> SignalHandlerId -> Connection
connected object handler =
  Connection
    { block = GObject.signalHandlerBlock object handler
    , unblock = GObject.signalHandlerUnblock object handler
    }

-- | Connect a handler with 'onSelf'.
connectSelf
  :: (GObject.IsObject o, SignalInfo info)
  => o
  -> SignalProxy o info
  -> ((?self :: o) => HaskellCallbackType info)
  -> IO Connection
connectSelf object signal handler = connected object <$> onSelf object signal handler

-- | Run the action with the handlers blocked.
quietly :: Connection -> IO r -> IO r
quietly connection = bracket_ connection.block connection.unblock

bind :: (Eq a) => Control a -> (a -> IO ()) -> IO (a -> IO ())
bind control react = fst <$> bindQuietly control react

-- | Paint only when 'current' differs. React only when an update changes
-- the value.
bindQuietly :: (Eq a) => Control a -> (a -> IO ()) -> IO (a -> IO (), IO () -> IO ())
bindQuietly control react = do
  held <- control.current >>= newIORef
  connection <- control.connect $ \update -> do
    old <- readIORef held
    let new = update old
    writeIORef held new
    when (new /= old) (react new)
  let paint wanted = do
        shown <- control.current
        writeIORef held shown
        when (shown /= wanted) $ do
          quietly connection (control.paint wanted)
          writeIORef held wanted
  pure (paint, quietly connection)

-- | Paint only when 'current' differs.
paintChanged :: (Eq a) => Control a -> a -> IO ()
paintChanged control wanted = do
  shown <- control.current
  when (shown /= wanted) (control.paint wanted)

-- | Drop updates whose result is rejected. A rejected 'entryApply' text
-- stays until the model changes.
accepting :: (a -> Bool) -> Control a -> Control a
accepting ok control =
  control
    { connect = \report ->
        control.connect (\update -> report (\old -> let new = update old in if ok new then new else old))
    }

-- | Each side reports only its own part, so no handler reads the other
-- widget.
pair :: (Eq a, Eq b) => Control a -> Control b -> Control (a, b)
pair left right =
  Control
    { paint = \(a, b) -> paintChanged left a >> paintChanged right b
    , connect = \report ->
        (<>) <$> left.connect (report . first) <*> right.connect (report . second)
    , current = (,) <$> left.current <*> right.current
    }

closing :: IO () -> Bool -> IO ()
closing close open = unless open close

-- | A control for one widget property.
propertyControl
  :: (GObject.IsObject w, SignalInfo info, HaskellCallbackType info ~ (a -> IO ()))
  => (w -> IO a)
  -> (w -> a -> IO ())
  -> SignalProxy w info
  -> w
  -> Control a
propertyControl getter setter signal widget =
  Control
    { paint = setter widget
    , connect = \report -> connectSelf widget signal (report . const)
    , current = getter widget
    }

switch :: Gtk.Switch -> Control Bool
switch = propertyControl Gtk.switchGetActive Gtk.switchSetActive (PropertySet #active)

switchRow :: Adw.SwitchRow -> Control Bool
switchRow = propertyControl Adw.switchRowGetActive Adw.switchRowSetActive (PropertySet #active)

comboRow :: Adw.ComboRow -> Control Word32
comboRow = propertyControl Adw.comboRowGetSelected Adw.comboRowSetSelected (PropertySet #selected)

dropDown :: Gtk.DropDown -> Control Word32
dropDown = propertyControl Gtk.dropDownGetSelected Gtk.dropDownSetSelected (PropertySet #selected)

toggleRadio :: (Eq a) => List (a, Gtk.ToggleButton) -> Control (Maybe a)
toggleRadio =
  radio Gtk.toggleButtonGetActive Gtk.toggleButtonSetActive (\button callback -> connectSelf button (PropertySet #active) callback)

checkRadio :: (Eq a) => List (a, Gtk.CheckButton) -> Control (Maybe a)
checkRadio =
  radio Gtk.checkButtonGetActive Gtk.checkButtonSetActive (\button callback -> connectSelf button (PropertySet #active) callback)

radio
  :: (Eq a)
  => (b -> IO Bool)
  -> (b -> Bool -> IO ())
  -> (b -> (Bool -> IO ()) -> IO Connection)
  -> List (a, b)
  -> Control (Maybe a)
radio getActive setActive onActive buttons =
  Control
    { paint = \case
        Nothing -> forM_ buttons (\(_, button) -> setActive button False)
        Just wanted -> forM_ buttons (\(value, button) -> when (value == wanted) (setActive button True))
    , connect = \report ->
        mconcat <$> forM buttons (\(value, button) -> onActive button (\active -> when active (report (const (Just value)))))
    , current = firstActive buttons
    }
  where
    firstActive = \case
      [] -> pure Nothing
      (value, button) : rest -> getActive button >>= \active -> if active then pure (Just value) else firstActive rest

listSelection
  :: Gtk.ListBox
  -> (Gtk.ListBoxRow -> IO (Maybe a))
  -> (a -> IO (Maybe Gtk.ListBoxRow))
  -> Control (Maybe a)
listSelection list keyOf rowOf =
  Control
    { paint = \case
        Nothing -> Gtk.listBoxUnselectAll list
        Just key -> rowOf key >>= mapM_ (Gtk.listBoxSelectRow list . Just)
    , connect = \report ->
        connectSelf list #rowSelected (\row -> join <$> traverse keyOf row >>= report . const)
    , current = Gtk.listBoxGetSelectedRow list >>= fmap join . traverse keyOf
    }

searchText :: Gtk.SearchEntry -> Control Text
searchText = propertyControl Gtk.editableGetText paintEditable (PropertySet #text)

-- | Reported on @#apply@. 'current' is the last painted text, so a
-- refresh keeps unapplied or rejected text.
entryApply :: Adw.EntryRow -> IO (Control Text)
entryApply row = do
  held <- Gtk.editableGetText row >>= newIORef
  pure
    Control
      { paint = \text -> writeIORef held text >> paintEditable row text
      , connect = \report ->
          connectSelf row #apply (Gtk.editableGetText ?self >>= report . const)
      , current = readIORef held
      }

dialog :: Adw.Dialog -> Adw.ApplicationWindow -> IO () -> IO (Control Bool)
dialog shown window presented = do
  wanted <- newIORef False
  pure
    Control
      { paint = \open -> do
          wasOpen <- readIORef wanted
          writeIORef wanted open
          if open
            then Adw.dialogPresent shown (Just window) >> presented
            else when wasOpen (void (Adw.dialogClose shown))
      , connect = \report -> connectSelf shown #closed $ do
          wasOpen <- readIORef wanted
          writeIORef wanted False
          when wasOpen (report (const False))
      , current = readIORef wanted
      }
