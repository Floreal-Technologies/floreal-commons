{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module Floreal.Commons.Gtk.CommandPalette
  ( PaletteEvents (..)
  , newCommandPalette
  ) where

import Data.GI.Base (AttrOp ((:=)), new, on, set)
import Data.Text qualified as T
import Data.Vector qualified as V
import GI.Adw qualified as Adw
import GI.GLib qualified as GLib
import GI.Gdk qualified as Gdk
import GI.Gtk qualified as Gtk

import Floreal.Commons.Gtk.Bind (bind, closing, dialog, searchText)
import Floreal.Commons.Gtk.Common (newCell, newLabel, paddedBox, renderCell)
import Floreal.Commons.Palette (Entry (..), Match (..), PaletteRow (..), PaletteView (..), Target (..))

-- | What the user does in the palette. 'run' gets the key of an enabled
-- entry only.
data PaletteEvents c = PaletteEvents
  { closed :: IO ()
  , queryChanged :: Text -> IO ()
  , run :: c -> IO ()
  }

data Parts c = Parts
  { search :: Gtk.SearchEntry
  , list :: Gtk.ListBox
  , placeholder :: Gtk.Label
  , scroll :: Gtk.ScrolledWindow
  , shown :: IORef (Vector (Adw.ActionRow, Entry c))
  }

-- | Build the palette. The function gives the accelerator label of a
-- key, if any. The result paints a view.
newCommandPalette
  :: (Eq c)
  => Adw.ApplicationWindow
  -> (c -> Maybe Text)
  -> PaletteEvents c
  -> IO (PaletteView c -> IO ())
newCommandPalette window accelLabel events = do
  shown <- newIORef V.empty
  search <- new Gtk.SearchEntry [#hexpand := True]
  placeholder <- newLabel "" [#marginTop := 24, #marginBottom := 24] ["dim-label"]
  list <- new Gtk.ListBox [#selectionMode := Gtk.SelectionModeSingle]
  Gtk.widgetAddCssClass list "boxed-list"
  Gtk.listBoxSetPlaceholder list (Just placeholder)
  scroll <-
    new
      Gtk.ScrolledWindow
      [ #child := list
      , #hscrollbarPolicy := Gtk.PolicyTypeNever
      , #propagateNaturalHeight := True
      , #maxContentHeight := 420
      ]
  box <- paddedBox Gtk.OrientationVertical 12 12
  Gtk.boxAppend box search
  Gtk.boxAppend box scroll
  dialog' <- new Adw.Dialog [#contentWidth := 520, #child := box]
  let parts = Parts {search, list, placeholder, scroll, shown}
  void $ on search #activate (runSelected parts events)
  void $ on search #stopSearch (void (Adw.dialogClose dialog'))
  void $ on list #rowSelected (\row -> forM_ row (revealRow parts))
  void $ on list #rowActivated (runRow parts events)
  keys <- new Gtk.EventControllerKey []
  void $ on keys #keyPressed $ \keyval _ _ -> moveSelection parts keyval
  Gtk.widgetAddController search keys
  openControl <- dialog dialog' window (void (Gtk.widgetGrabFocus search))
  paintOpen <- bind openControl (closing events.closed)
  paintQuery <- bind (searchText search) events.queryChanged
  itemsCell <- newCell (paintItems parts accelLabel)
  pure $ \view -> do
    renderCell itemsCell (view.items, view.placeholder, view.noMatch)
    paintQuery view.query
    paintOpen view.open

paintItems :: Parts c -> (c -> Maybe Text) -> (List (PaletteRow c), Text, Text) -> IO ()
paintItems parts accelLabel (items, placeholder, noMatch) = do
  set parts.search [#placeholderText := placeholder]
  set parts.placeholder [#label := noMatch]
  Gtk.listBoxRemoveAll parts.list
  built <- V.fromList <$> traverse (newRow accelLabel) items
  V.forM_ built (\(row, _) -> Gtk.listBoxAppend parts.list row)
  writeIORef parts.shown built
  V.forM_ (V.take 1 built) (\(row, _) -> Gtk.listBoxSelectRow parts.list (Just row))

newRow :: (c -> Maybe Text) -> PaletteRow c -> IO (Adw.ActionRow, Entry c)
newRow accelLabel item = do
  title <- case item.match.target of
    OnLabel -> emphasize item.match.hits item.entry.label
    OnIdentifier -> GLib.markupEscapeText item.entry.label (-1)
  built <-
    new
      Adw.ActionRow
      [ #useMarkup := True
      , #title := title
      , #subtitle := item.entry.identifier
      , #activatable := True
      , #sensitive := item.entry.enabled
      ]
  forM_ (accelLabel item.entry.key) $ \text -> do
    suffix <- newLabel text [] ["dim-label"]
    Adw.actionRowAddSuffix built suffix
  pure (built, item.entry)

emphasize :: List Int -> Text -> IO Text
emphasize hits label = T.concat <$> traverse piece (zip [0 ..] (T.unpack label))
  where
    piece (index, c) = do
      escaped <- GLib.markupEscapeText (T.singleton c) (-1)
      pure (if index `elem` hits then "<b>" <> escaped <> "</b>" else escaped)

revealRow :: Parts c -> Gtk.ListBoxRow -> IO ()
revealRow parts row = do
  index <- Gtk.listBoxRowGetIndex row
  rows <- readIORef parts.shown
  above <- V.mapM (\(built, _) -> Gtk.widgetGetHeight built) (V.take (fromIntegral index) rows)
  height <- Gtk.widgetGetHeight row
  let top = fromIntegral (V.sum above)
  vadj <- Gtk.scrolledWindowGetVadjustment parts.scroll
  Gtk.adjustmentClampPage vadj top (top + fromIntegral height)

runSelected :: Parts c -> PaletteEvents c -> IO ()
runSelected parts events = do
  selected <- Gtk.listBoxGetSelectedRow parts.list
  forM_ selected (runRow parts events)

runRow :: Parts c -> PaletteEvents c -> Gtk.ListBoxRow -> IO ()
runRow parts events row = do
  index <- Gtk.listBoxRowGetIndex row
  rows <- readIORef parts.shown
  forM_ (rows V.!? fromIntegral index) $ \(_, picked) ->
    when picked.enabled (events.run picked.key)

moveSelection :: Parts c -> Word32 -> IO Bool
moveSelection parts keyval
  | keyval == Gdk.KEY_Down = step 1
  | keyval == Gdk.KEY_Up = step (-1)
  | otherwise = pure False
  where
    step :: Int32 -> IO Bool
    step offset = do
      current <- Gtk.listBoxGetSelectedRow parts.list
      index <- maybe (pure (-1)) Gtk.listBoxRowGetIndex current
      next <- Gtk.listBoxGetRowAtIndex parts.list (max 0 (index + offset))
      forM_ next (Gtk.listBoxSelectRow parts.list . Just)
      pure True
