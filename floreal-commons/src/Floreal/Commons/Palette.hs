{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module Floreal.Commons.Palette
  ( Entry (..)
  , Target (..)
  , Match (..)
  , PaletteRow (..)
  , PaletteView (..)
  , fold
  , matchText
  , rank
  , paletteView
  ) where

import Data.Char (GeneralCategory (NonSpacingMark), generalCategory, toLower)
import Data.Containers.ListUtils (nubOrd)
import Data.Ord (Down (..))
import Data.Text qualified as T
import Data.Text.Normalize (NormalizationMode (NFD), normalize)
import Data.Vector qualified as V

data Entry c = Entry
  { key :: c
  , identifier :: Text
  , label :: Text
  , enabled :: Bool
  }
  deriving stock (Eq, Show)

data Target = OnLabel | OnIdentifier
  deriving stock (Eq, Show)

data Match = Match
  { score :: Int
  , target :: Target
  , hits :: List Int
  }
  deriving stock (Eq, Show)

foldChar :: Char -> List Char
foldChar c
  | c == '-' = [' ']
  | otherwise =
      T.unpack (normalize NFD (T.singleton c))
        & filter (\d -> generalCategory d /= NonSpacingMark)
        & map toLower

fold :: Text -> Text
fold = T.pack . concatMap foldChar . T.unpack

indexed :: Text -> Vector (Int, Char)
indexed text = V.fromList (concat (zipWith (\i c -> map (\d -> (i, d)) (foldChar c)) [0 ..] (T.unpack text)))

matchText :: Target -> Text -> Text -> Maybe Match
matchText target query candidate
  | T.null needle = Just Match {score = 0, target, hits = []}
  | otherwise = do
      found <- walk (T.unpack needle) 0
      let bonus = if needle `T.isInfixOf` T.pack (map snd (V.toList stream)) then 10 else 0
      pure
        Match
          { score = sum (zipWith hitScore (Nothing : map Just found) found) + bonus
          , target
          , hits = nubOrd (map (\position -> fst (stream V.! position)) found)
          }
  where
    needle = T.strip (fold query)
    stream = indexed candidate
    walk :: List Char -> Int -> Maybe (List Int)
    walk [] _ = Just []
    walk (q : qs) from = do
      position <- V.findIndex (\(_, c) -> c == q) (V.drop from stream)
      let here = from + position
      rest <- walk qs (here + 1)
      pure (here : rest)
    hitScore :: Maybe Int -> Int -> Int
    hitScore previous here
      | here == 0 || snd (stream V.! (here - 1)) == ' ' = 3
      | previous == Just (here - 1) = 2
      | otherwise = 1

bestMatch :: Text -> Entry c -> Maybe Match
bestMatch query entry =
  listToMaybe (sortOn (Down . (.score)) (catMaybes [matchText OnLabel query entry.label, matchText OnIdentifier query entry.identifier]))

data PaletteRow c = PaletteRow
  { entry :: Entry c
  , match :: Match
  }
  deriving stock (Eq, Show)

rank :: Text -> List (Entry c) -> List (PaletteRow c)
rank query entries =
  sortOn (\row -> Down row.match.score) (mapMaybe (\entry -> PaletteRow entry <$> bestMatch query entry) entries)

data PaletteView c = PaletteView
  { open :: Bool
  , query :: Text
  , items :: List (PaletteRow c)
  , placeholder :: Text
  , noMatch :: Text
  }
  deriving stock (Eq, Show)

paletteView :: Text -> Text -> List (Entry c) -> Maybe Text -> PaletteView c
paletteView placeholder noMatch entries palette =
  PaletteView
    { open = isJust palette
    , query = fromMaybe "" palette
    , items = maybe [] (`rank` entries) palette
    , placeholder
    , noMatch
    }
