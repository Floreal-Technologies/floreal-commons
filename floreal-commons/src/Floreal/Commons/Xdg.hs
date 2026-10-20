{-# LANGUAGE TypeFamilies #-}

module Floreal.Commons.Xdg
  ( Xdg (..)
  , XdgDirectory (..)
  , XdgDirectoryList (..)
  , getXdgDirectory
  , getXdgDirectoryList
  , appPath
  , runXdgIO
  ) where

import Effectful (Dispatch (Dynamic), DispatchOf)
import Effectful.Dispatch.Dynamic (interpret_, send)
import System.Directory.OsPath (XdgDirectory (..), XdgDirectoryList (..))
import System.Directory.OsPath qualified as Directory
import System.OsPath ((</>))

data Xdg :: Effect where
  GetXdgDirectory :: XdgDirectory -> OsPath -> Xdg m OsPath
  GetXdgDirectoryList :: XdgDirectoryList -> Xdg m (List OsPath)

type instance DispatchOf Xdg = Dynamic

getXdgDirectory :: (Xdg :> es) => XdgDirectory -> OsPath -> Eff es OsPath
getXdgDirectory kind program = send (GetXdgDirectory kind program)

getXdgDirectoryList :: (Xdg :> es) => XdgDirectoryList -> Eff es (List OsPath)
getXdgDirectoryList kind = send (GetXdgDirectoryList kind)

-- | A path in the XDG directory of a program, for example
-- @appPath XdgState [osp|mediacopy3000|] [osp|jobs|]@.
appPath :: (Xdg :> es) => XdgDirectory -> OsPath -> OsPath -> Eff es OsPath
appPath kind program path = getXdgDirectory kind program <&> (</> path)

runXdgIO :: (IOE :> es) => Eff (Xdg : es) a -> Eff es a
runXdgIO = interpret_ $ \case
  GetXdgDirectory kind program -> liftIO (Directory.getXdgDirectory kind program)
  GetXdgDirectoryList kind -> liftIO (Directory.getXdgDirectoryList kind)
