module Hyper.RouterSpec where

import Prelude
import Data.Tuple (Tuple(Tuple))
import Hyper.BodyParser (parse)
import Hyper.Conn (ResponseMiddleware, HTTP)
import Hyper.Form (FormParser(FormParser), Form(Form))
import Hyper.Router (router)
import Hyper.Stream (fromString)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.Aff (expectError)

helloHandler :: forall e res b c.
                ResponseMiddleware e { body :: b | res } { body :: String | res } c
helloHandler conn@{ response: r } = 
  pure (conn { response = ( r { body = "Hello!"}) })

spec :: forall e. Spec (http :: HTTP | e) Unit
spec =
  describe "Hyper.Router" do
    it "can route the requests" do
      conn <- router
              {
                "GET": helloHandler
              }
              { request: { body: fromString ""
                           -- Headers required by formParser (content-type, content-length):
                         , headers: { "content-type": "application/x-www-form-urlencoded; charset=utf8"
                                    , "content-length": "7"
                                    -- Other headers are OK too.
                                    , "host": "localhost"
                                    , "user-agent": "test"
                                    }
                         }
              , response: {}
              , components: {}
              }
      conn.response.body `shouldEqual` "Hello!"
