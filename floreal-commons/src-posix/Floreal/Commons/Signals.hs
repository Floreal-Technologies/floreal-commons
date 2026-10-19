module Floreal.Commons.Signals (onStopSignal) where

import System.Posix.Signals (Handler (Catch), installHandler, sigHUP, sigINT, sigTERM)

onStopSignal :: IO () -> IO ()
onStopSignal action = for_ [sigINT, sigTERM, sigHUP] (\sig -> installHandler sig (Catch action) Nothing)
