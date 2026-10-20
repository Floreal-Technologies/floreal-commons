module Floreal.Commons.Process.Native
  ( Group
  , groupOf
  , terminateGroup
  , killGroup
  , killChild
  , nullDevice
  ) where

import System.Process (ProcessHandle, terminateProcess)

data Group = Group

groupOf :: ProcessHandle -> IO Group
groupOf _ = pure Group

terminateGroup :: ProcessHandle -> Group -> IO ()
terminateGroup handle _ = stop handle

killGroup :: ProcessHandle -> Group -> IO ()
killGroup handle _ = stop handle

killChild :: ProcessHandle -> IO ()
killChild = stop

stop :: ProcessHandle -> IO ()
stop handle = void (try @IOException (terminateProcess handle))

nullDevice :: FilePath
nullDevice = "\\\\.\\NUL"
