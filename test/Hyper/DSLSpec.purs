module Hyper.HTML.DSLSpec where

import Prelude
import Hyper.Conn (HTTP)
import Hyper.HTML.DSL (text, linkTo, html)
import Hyper.Method (Method(GET))
import Hyper.Router (Route(Route), router, class Routable)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

data MyRoutes
  = About
  | Contact

instance routableMyRoutes :: Routable MyRoutes where
  fromPath url =
    case url of
      Route GET "/about" -> About
      Route GET "/contact" -> Contact
      -- TODO: Error handling, as this is not total
      Route _ _ -> Contact
  toPath routes =
    case routes of
      About -> Route GET "/about"
      Contact -> Route GET "/contact"

spec :: forall e. Spec (http :: HTTP | e) Unit
spec = do
  let route r =
        case r of
          About -> html $
            linkTo Contact (text "Contact Me!")
          Contact -> html $
            linkTo About (text "About Me")

  describe "Hyper.HTML.DSL" do
    it "can linkTo an existing route" do
      conn <- (router route)
              { request: { method: GET
                         , path: "/about"
                         }
              , response: {}
              , components: {}
              }
      conn.response.body `shouldEqual` "<a href=\"/contact\">Contact Me!</a>"

    it "can linkTo another existing route" do
      conn <- (router route)
              { request: { method: GET
                         , path: "/contact"
                         }
              , response: {}
              , components: {}
              }
      conn.response.body `shouldEqual` "<a href=\"/about\">About Me</a>"

