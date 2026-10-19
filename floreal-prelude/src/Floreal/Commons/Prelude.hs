module Floreal.Commons.Prelude
  ( module Prelude

    -- * Types
  , Text
  , Map
  , Set
  , ByteString
  , List
  , NonEmpty (..)
  , OsPath
  , Int8
  , Int16
  , Int32
  , Int64
  , Word8
  , Word16
  , Word32
  , Word64
  , Vector

    -- * Effectful
  , Eff
  , Effect
  , IOE
  , type (:>)
  , liftIO

    -- * Display
  , Display (..)
  , display

    -- * Control
  , forM_
  , forM
  , for_
  , traverse_
  , when
  , unless
  , void
  , forever
  , join
  , foldM
  , (>=>)

    -- * Combinators
  , (&)
  , (<&>)
  , ($>)
  , first
  , second
  , bimap

    -- * Maybe and Either
  , fromMaybe
  , mapMaybe
  , catMaybes
  , maybeToList
  , listToMaybe
  , isJust
  , isNothing
  , partitionEithers
  , isLeft
  , isRight

    -- * Lists
  , sort
  , sortOn
  , find

    -- * IORef
  , IORef
  , newIORef
  , readIORef
  , writeIORef
  , modifyIORef'
  , atomicModifyIORef'

    -- * Exceptions
  , try
  , throwIO
  , bracket
  , finally
  , SomeException
  , IOException
  ) where

import Control.Exception (IOException, SomeException, bracket, finally, throwIO, try)
import Control.Monad (foldM, forM, forM_, forever, join, unless, void, when, (>=>))
import Data.Bifunctor (bimap, first, second)
import Data.ByteString (ByteString)
import Data.Either (isLeft, isRight, partitionEithers)
import Data.Foldable (for_, traverse_)
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (List, find, sort, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Display (Display (..), display)
import Data.Vector (Vector)
import Data.Word (Word16, Word32, Word64, Word8)
import Effectful (Eff, Effect, IOE, liftIO, type (:>))
import System.OsPath (OsPath)
import Prelude hiding
  ( appendFile
  , getContents
  , getLine
  , head
  , interact
  , putStr
  , putStrLn
  , readFile
  , tail
  , writeFile
  , (!!)
  )
