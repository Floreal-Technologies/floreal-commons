{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Small helpers for GTK widgets.
module Floreal.Commons.Gtk.Common
  ( Cell
  , newCell
  , renderCell
  , toggleClass
  , paintEditable
  , paddedBox
  , newLabel
  ) where

import Data.GI.Base (AttrOp ((:=)), new)
import Data.GI.Base.Attributes (AttrOpTag (AttrConstruct))
import GI.Gtk qualified as Gtk

-- | A paint function that runs only when its value changes.
data Cell a
  = Cell
      (IORef (Maybe a))
      (a -> IO ())

newCell :: (a -> IO ()) -> IO (Cell a)
newCell paint = do
  ref <- newIORef Nothing
  pure (Cell ref paint)

renderCell
  :: (Eq a)
  => Cell a
  -> a
  -> IO ()
renderCell (Cell ref paint) wanted = do
  painted <- readIORef ref
  when (painted /= Just wanted) $ do
    writeIORef ref (Just wanted)
    paint wanted

toggleClass
  :: (Gtk.IsWidget w)
  => w
  -> Text
  -> Bool
  -> IO ()
toggleClass widget className wanted =
  if wanted
    then Gtk.widgetAddCssClass widget className
    else Gtk.widgetRemoveCssClass widget className

-- | Set the text only when it is different, so the cursor stays.
paintEditable :: (Gtk.IsEditable w) => w -> Text -> IO ()
paintEditable editable text = do
  shown <- Gtk.editableGetText editable
  when (shown /= text) (Gtk.editableSetText editable text)

paddedBox :: Gtk.Orientation -> Int32 -> Int32 -> IO Gtk.Box
paddedBox orientation spacing margin =
  new
    Gtk.Box
    [ #orientation := orientation
    , #spacing := spacing
    , #marginTop := margin
    , #marginBottom := margin
    , #marginStart := margin
    , #marginEnd := margin
    ]

newLabel :: Text -> List (AttrOp Gtk.Label 'AttrConstruct) -> List Text -> IO Gtk.Label
newLabel text attrs classes = do
  label <- new Gtk.Label ((#label := text) : attrs)
  forM_ classes (Gtk.widgetAddCssClass label)
  pure label
