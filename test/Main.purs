module Test.Main where

import Prelude
import Effect (Effect)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main :: Effect Unit
main = discover "Hyper\\..*Spec" >>= run [consoleReporter]
