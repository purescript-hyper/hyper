module Hyper.Test.TestServer where

import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind.Indexed (ibind)
import Control.Monad (class Monad, void)
import Control.Monad.Indexed (ipure, (:*>))
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Control.Monad.Writer.Class (class MonadTell)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Function ((<<<))
import Data.Functor (map)
import Data.HTTP.Method (CustomMethod, Method(..))
import Data.Lazy (defer)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Monoid (mempty, class Monoid)
import Data.Newtype (class Newtype, unwrap)
import Data.Semigroup (class Semigroup, (<>))
import Data.String as String
import Foreign.Object (Object)
import Foreign.Object as Object
import Hyper.Conn (Conn, kind RequestState, kind ResponseState)
import Hyper.Header (Header)
import Hyper.Middleware (lift')
import Hyper.Middleware.Class (getConn, modifyConn, putConn)
import Hyper.Request (class ReadableBody, class Request, parseUrl)
import Hyper.Response (class ResponseWritable, class Response)
import Hyper.Status (Status)

-- REQUEST

newtype TestRequest (reqState :: RequestState)
  = TestRequest { url :: String
                , method :: Either Method CustomMethod
                , body :: String
                , headers :: Object String
                }

defaultRequest :: { url :: String
                  , method :: Either Method CustomMethod
                  , body :: String
                  , headers :: Object String
                  }
defaultRequest =
  { url: ""
  , method: Left GET
  , body: ""
  , headers: Object.empty
  }

instance readableBodyStringBody :: Monad m
                                => ReadableBody TestRequest m String where
  readBody = let bind = ibind in do
    conn <- getConn
    let TestRequest rec = conn.request
    _ <- putConn (conn { request = TestRequest rec })
    ipure rec.body

instance requestTestRequest :: Monad m => Request TestRequest m where
  getRequestData = let bind = ibind in do
    conn <- getConn
    let TestRequest r = conn.request
    _ <- putConn (conn { request = TestRequest r })
    ipure { url: r.url
          , parsedUrl: defer \_ -> parseUrl r.url
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

data TestResponse b (resState :: ResponseState)
  = TestResponse (Maybe Status) (Array Header) (Array b)

testStatus :: forall b resState. TestResponse b resState → Maybe Status
testStatus (TestResponse status _ _) = status

testHeaders :: forall b resState. TestResponse b resState → Array Header
testHeaders (TestResponse _ headers _) = headers

testBodyChunks :: forall b resState. TestResponse b resState → Array b
testBodyChunks (TestResponse _ _ body) = body

testBody :: forall b resState. Monoid b => TestResponse b resState → b
testBody (TestResponse _ _ body) = fold body

instance semigroupTestResponse :: Semigroup (TestResponse b resState) where
  append (TestResponse status headers bodyChunks) (TestResponse status' headers' bodyChunks') =
    TestResponse (status <|> status') (headers <> headers') (bodyChunks <> bodyChunks')

instance monoidTestResponse :: Monoid (TestResponse b resState) where
  mempty = TestResponse Nothing [] []

testStringBody :: forall resState. TestResponse StringBody resState → String
testStringBody (TestResponse _ _ chunks) = fold (map unwrap chunks)


-- SERVER


testServer
  :: forall m a b resState
   . Monad m
   => WriterT (TestResponse b resState) m a
   -> m (TestResponse b resState)
testServer = execWriterT <<< void


resetResponse
  :: forall req c body reqState fromResponse toResponse
   . Conn req reqState (TestResponse body) fromResponse c
  -> Conn req reqState (TestResponse body) toResponse c
resetResponse conn@{ response: TestResponse status headers body } =
  conn { response = TestResponse status headers body }

instance responseWriterTestResponse :: ( Monad m
                                       , MonadTell (TestResponse b resState) m
                                       ) =>
                                       Response
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
