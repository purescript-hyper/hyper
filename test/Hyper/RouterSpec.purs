module Hyper.RouterSpec where

import Prelude
import Hyper.Conn (HTTP)
import Hyper.Method (Method(POST, GET))
import Hyper.Response (StringResponse(StringResponse), respond)
import Hyper.Router (Route(Route), router, class Routable)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

data MyRoutes
  = GetGreeting
  | SaveGreeting

instance routableMyRoutes :: Routable MyRoutes where
  fromPath url =
    case url of
      Route GET [] -> GetGreeting
      Route POST [] -> SaveGreeting
      -- TODO: Error handling, as this is not total
      Route _ _ -> SaveGreeting
  toPath routes =
    case routes of
      GetGreeting -> Route GET []
      SaveGreeting -> Route POST []

spec :: forall e. Spec (http :: HTTP | e) Unit
spec = do
  let route r =
        case r of
          GetGreeting -> respond (StringResponse "Hello!")
          SaveGreeting -> respond (StringResponse "OK, I've saved that for ya.")

  describe "Hyper.Router" do
    it "can route a GET for the root resource" do
      conn <- (router route)
              { request: { method: GET
                         , path: "/"
                         }
              , response: {}
              , components: {}
              }
      conn.response.body `shouldEqual` "Hello!"

    it "can route a POST for the root resource" do
      conn <- (router route)
              { request: { method: POST
                         , path: ""
                         }
              , response: {}
              , components: {}
              }
      conn.response.body `shouldEqual` "OK, I've saved that for ya."
