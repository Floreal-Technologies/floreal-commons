module Floreal.Commons.Xdg
  ( XdgDirectory (..)
  , appPath
  , createAppDirectory
  ) where

import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.OsPath (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)
import System.OsPath ((</>))

-- | A path in the XDG directory of a program, for example
-- @appPath XdgState [osp|mediacopy3000|] [osp|jobs|]@.
appPath :: (FileSystem :> es) => XdgDirectory -> OsPath -> OsPath -> Eff es OsPath
appPath kind program path = (</> path) <$> getXdgDirectory kind program

-- | 'appPath' for a directory. The directory and its parents are
-- created if they are missing.
createAppDirectory :: (FileSystem :> es) => XdgDirectory -> OsPath -> OsPath -> Eff es OsPath
createAppDirectory kind program path = do
  directory <- appPath kind program path
  createDirectoryIfMissing True directory
  pure directory
