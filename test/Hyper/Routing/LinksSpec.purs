module Hyper.Routing.LinksSpec (spec) where

import Prelude
import Data.URI (printURI)
import Hyper.Routing ((:<|>))
import Hyper.Routing.Links (linksTo)
import Hyper.Routing.TestSite (UserID(..), testSite)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: forall e. Spec e Unit
spec = do
  describe "Hyper.Routing.Links" $
    describe "linksTo" $

      case linksTo testSite of
        (homeUri :<|> userLinks :<|> wikiUri) -> do

          it "returns link for Lit" $
            printURI homeUri `shouldEqual` "/"

          it "returns link for nested routes" $
            case userLinks (UserID "owi") of
              (profileUri :<|> friendsUri) -> do
                  printURI profileUri `shouldEqual` "/users/owi/profile"
                  printURI friendsUri `shouldEqual` "/users/owi/friends"

          it "returns link for CaptureAll" $
            printURI (wikiUri ["foo", "bar", "baz.txt"]) `shouldEqual` "/wiki/foo/bar/baz.txt"

          -- it "returns link for Raw" $
            -- printURI aboutUri `shouldEqual` "/about"
