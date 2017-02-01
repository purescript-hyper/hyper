module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Node.FS (FS)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects (fs :: FS)) Unit
main = discover "Hyper\\..*Spec" >>= run [consoleReporter]
