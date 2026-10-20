{-# LANGUAGE TypeFamilies #-}

module Floreal.Commons.Environment
  ( Environment (..)
  , lookupEnv
  , getEnvironment
  , getArgs
  , getExecutablePath
  , getHomeDirectory
  , getHostName
  , runEnvironmentIO
  ) where

import Data.Text qualified as T
import Effectful (Dispatch (Dynamic), DispatchOf)
import Effectful.Dispatch.Dynamic (interpret_, send)
import Network.HostName qualified as HostName
import System.Directory.OsPath qualified as Directory
import System.Environment qualified as Env
import System.OsPath qualified as OsPath

data Environment :: Effect where
  LookupEnv :: String -> Environment m (Maybe String)
  GetEnvironment :: Environment m (List (String, String))
  GetArgs :: Environment m (List String)
  GetExecutablePath :: Environment m OsPath
  GetHomeDirectory :: Environment m OsPath
  GetHostName :: Environment m Text

type instance DispatchOf Environment = Dynamic

lookupEnv :: (Environment :> es) => String -> Eff es (Maybe String)
lookupEnv name = send (LookupEnv name)

getEnvironment :: (Environment :> es) => Eff es (List (String, String))
getEnvironment = send GetEnvironment

getArgs :: (Environment :> es) => Eff es (List String)
getArgs = send GetArgs

getExecutablePath :: (Environment :> es) => Eff es OsPath
getExecutablePath = send GetExecutablePath

getHomeDirectory :: (Environment :> es) => Eff es OsPath
getHomeDirectory = send GetHomeDirectory

getHostName :: (Environment :> es) => Eff es Text
getHostName = send GetHostName

runEnvironmentIO :: (IOE :> es) => Eff (Environment : es) a -> Eff es a
runEnvironmentIO = interpret_ $ \case
  LookupEnv name -> liftIO (Env.lookupEnv name)
  GetEnvironment -> liftIO Env.getEnvironment
  GetArgs -> liftIO Env.getArgs
  GetExecutablePath -> liftIO (Env.getExecutablePath >>= OsPath.encodeFS)
  GetHomeDirectory -> liftIO Directory.getHomeDirectory
  GetHostName -> liftIO (T.pack <$> HostName.getHostName)
