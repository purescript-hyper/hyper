module Hyper.RouterSpec where

import Prelude
import Hyper.Conn (Conn, Middleware, ResponseMiddleware, HTTP)
import Hyper.Method (Method(POST, GET))
import Hyper.Router (Route(Route), router, class Routable)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

respond :: forall e res b c. String
           -> ResponseMiddleware e { body :: b | res } { body :: String | res } c
respond s c = 
  pure (c { response = ( c.response { body = s }) })


data MyRoutes
  = GetGreeting
  | SaveGreeting

instance routableMyRoutes :: Routable MyRoutes where
  fromPath url =
    case url of
      Route GET "/" -> GetGreeting
      Route POST "/" -> SaveGreeting
      Route _ _ -> SaveGreeting
  toPath routes =
    case routes of
      GetGreeting -> Route GET "/"
      SaveGreeting -> Route POST "/"

route r =
  case r of
    GetGreeting -> respond "Hello!"
    SaveGreeting -> respond "OK, I've saved that for ya."

spec :: forall e. Spec (http :: HTTP | e) Unit
spec =
  describe "Hyper.Router" do
    it "can route a GET for the root resource" do
      conn <- (router route)
              { request: { method: GET
                         , path: "/"
                         }
              , response: { body: {} }
              , components: {}
              }
      conn.response.body `shouldEqual` "Hello!"

    it "can route a POST for the root resource" do
      conn <- (router route)
              { request: { method: POST
                         , path: ""
                         }
              , response: { body: {} }
              , components: {}
              }
      conn.response.body `shouldEqual` "OK, I've saved that for ya."

