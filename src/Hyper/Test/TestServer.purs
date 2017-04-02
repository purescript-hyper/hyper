module Hyper.Test.TestServer where

import Data.String as String
import Data.StrMap as StrMap
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.IxMonad (ipure, (:*>), (:>>=))
import Control.Monad (class Monad, void)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Control.Monad.Writer.Class (class MonadTell)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Function ((<<<))
import Data.Functor (map)
import Data.HTTP.Method (CustomMethod, Method(..))
import Data.Maybe (Maybe(Nothing, Just))
import Data.Monoid (mempty, class Monoid)
import Data.Newtype (class Newtype, unwrap)
import Data.Semigroup (class Semigroup, (<>))
import Data.StrMap (StrMap)
import Hyper.Conn (Conn)
import Hyper.Header (Header)
import Hyper.Middleware (lift')
import Hyper.Middleware.Class (getConn, modifyConn)
import Hyper.Request (class ReadableBody, class Request)
import Hyper.Response (class ResponseWritable, class ResponseWriter)
import Hyper.Status (Status)

-- REQUEST

newtype TestRequest
  = TestRequest { url :: String
                , method :: Either Method CustomMethod
                , body :: String
                , headers :: StrMap String
                }

defaultRequest :: { url :: String
                  , method :: Either Method CustomMethod
                  , body :: String
                  , headers :: StrMap String
                  }
defaultRequest =
  { url: ""
  , method: Left GET
  , body: ""
  , headers: StrMap.empty
  }

instance readableBodyStringBody :: Monad m
                                => ReadableBody TestRequest m String where
  readBody = getConn :>>= \{ request: TestRequest { body }} -> pure body

instance requestTestRequest :: Monad m => Request TestRequest m where
  getRequestData =
    getConn :>>= \{ request: TestRequest r } ->
    ipure { url: r.url
          , contentLength: Just (String.length r.body)
          , method: r.method
          , headers: r.headers
          }

-- RESPONSE BODY

newtype StringBody = StringBody String

derive instance newtypeStringBody :: Newtype StringBody _

instance responseStringBody :: Monad m => ResponseWritable StringBody m String where
  toResponse = pure <<< StringBody

instance semigroupStringBody :: Semigroup StringBody where
  append (StringBody s) (StringBody s') =
    StringBody (s <> s')

instance monoidStringBody :: Monoid StringBody where
  mempty = StringBody ""

-- RESPONSE

data TestResponse b state
  = TestResponse (Maybe Status) (Array Header) (Array b)

testStatus :: forall b state. TestResponse b state → Maybe Status
testStatus (TestResponse status _ _) = status

testHeaders :: forall b state. TestResponse b state → Array Header
testHeaders (TestResponse _ headers _) = headers

testBodyChunks :: forall b state. TestResponse b state → Array b
testBodyChunks (TestResponse _ _ body) = body

testBody :: forall b state. Monoid b => TestResponse b state → b
testBody (TestResponse _ _ body) = fold body

instance semigroupTestResponse :: Semigroup (TestResponse b state) where
  append (TestResponse status headers bodyChunks) (TestResponse status' headers' bodyChunks') =
    TestResponse (status <|> status') (headers <> headers') (bodyChunks <> bodyChunks')

instance monoidTestResponse :: Monoid (TestResponse b state) where
  mempty = TestResponse Nothing [] []

testStringBody :: forall state. TestResponse StringBody state → String
testStringBody (TestResponse _ _ chunks) = fold (map unwrap chunks)


-- SERVER


testServer
  :: forall m a b state
   . Monad m
   => WriterT (TestResponse b state) m a
   -> m (TestResponse b state)
testServer = execWriterT <<< void


resetResponse
  :: forall req c body a b
   . Conn req (TestResponse body a) c
  -> Conn req (TestResponse body b) c
resetResponse conn@{ response: TestResponse status headers body } =
  conn { response = TestResponse status headers body }

instance responseWriterTestResponse :: ( Monad m
                                       , MonadTell (TestResponse b state) m
                                       ) =>
                                       ResponseWriter
                                       (TestResponse b)
                                       m
                                       b where
  writeStatus status = do
    lift' (tell (TestResponse (Just status) [] []))
    :*> modifyConn resetResponse

  writeHeader header =
    lift' (tell (TestResponse Nothing [header] mempty))

  closeHeaders = modifyConn resetResponse

  send chunk =
    lift' (tell (TestResponse Nothing [] [chunk]))

  end = modifyConn resetResponse
