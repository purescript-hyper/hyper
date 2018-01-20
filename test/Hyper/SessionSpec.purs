module Hyper.SessionSpec where

import Prelude
import Control.IxMonad ((:*>))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Writer.Trans (WriterT, execWriterT, runWriterT)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..), fst)
import Hyper.Conn (Conn)
import Hyper.Cookies (Values, cookies)
import Hyper.Middleware (Middleware, evalMiddleware, runMiddleware)
import Hyper.Node.Session.Cookie (CookieStore(..), mkSecret)
import Hyper.Node.Session.InMemory (newInMemorySessionStore)
import Hyper.Response (HeadersOpen, class Response)
import Hyper.Session (class SessionStore, delete, deleteSession, get, getSession, newSessionID, put, saveSession)
import Hyper.Test.TestServer (TestRequest(..), TestResponse(..), defaultRequest)
import Node.Buffer (BUFFER)
import Node.Crypto (CRYPTO)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

type MyAff b state e = WriterT (TestResponse b state) (Aff e)

saveSession' ::
  forall b state e req res store c session.
  Response res (MyAff b state e) b  =>
  SessionStore store (MyAff b state e) session  =>
  session ->
  Middleware
  (MyAff b state e)
  (Conn
    req
    (res HeadersOpen)
    { sessions :: { key :: String, store :: store }
    , cookies :: Either String (StrMap.StrMap Values)
    | c })
  (Conn
    req
    (res HeadersOpen)
    { sessions :: { key :: String, store :: store }
    , cookies :: Either String (StrMap.StrMap Values)
    | c })
  Unit
saveSession' = saveSession

run :: forall w m a. Functor m => WriterT w m a -> m a
run = runWriterT >>> map fst

getCookie :: Array (Tuple String String) -> String
getCookie [Tuple "Set-Cookie" c] = c
getCookie  _ = ""

testStore :: forall store session e b state e.
  SessionStore store (MyAff b state (console :: CONSOLE | e)) session =>
  Show session =>
  Eq session =>
  Monoid session =>
  Aff (console :: CONSOLE | e) store -> session -> session -> Spec (console :: CONSOLE | e) Unit
testStore store session session' = do
  it "retrieves data that was stored" do
    store' <- store
    liftAff (session `shouldNotEqual` session')
    id <- run $ newSessionID store'
    id' <- run $ put store' id session
    sessionOut <- run $ get store' id'
    sessionOut `shouldEqual` Just session
    id1 <- run $ newSessionID store'
    id1' <- run $ put store' id1 session'
    sessionOut' <- run $ get store' id1'
    sessionOut' `shouldEqual` Just session'
    sessionOutSecond <- run $ get store' id'
    sessionOutSecond `shouldEqual` Just session
    id2 <- run $ newSessionID store'
    blankSession <- run $ get store' id2
    blankSession `shouldEqual` Nothing
    run $ delete store' id'
    sessionOutDeleted <- run $ get store' id
    sessionOutDeleted `shouldEqual` Nothing
  it "works with getSession/saveSession/deleteSession" do
    store' <- store
    Tuple sessionOut _ <- { request: TestRequest defaultRequest
                          , response: TestResponse Nothing [] []
                          , components: { sessions: { key: "session", store: store' }
                                        , cookies: unit }}
                          # runMiddleware (cookies :*> getSession) >>> run
    sessionOut `shouldEqual` Nothing
    TestResponse _ headers _ <- { request: TestRequest defaultRequest
                                , response: TestResponse Nothing [] []
                                , components: { sessions: { key: "session", store: store' }
                                              , cookies: unit }}
                                # evalMiddleware (cookies :*> saveSession' session) >>> execWriterT
    let newCookies = getCookie headers
    Tuple sessionOut' _ <- { request: TestRequest defaultRequest { headers = StrMap.singleton "cookie" newCookies }
                           , response: TestResponse Nothing [] []
                           , components: { sessions: { key: "session", store: store' }
                                         , cookies: unit }}
                           # runMiddleware (cookies :*> getSession) >>> run
    sessionOut' `shouldEqual` Just session
    Tuple response (TestResponse _ headers' _)
      <- { request: TestRequest defaultRequest { headers = StrMap.singleton "cookie" newCookies }
         , response: TestResponse Nothing [] []
         , components: { sessions: { key: "session", store: store' }
                       , cookies: unit }}
         # evalMiddleware (cookies :*> getSession :*> deleteSession) >>> runWriterT
    let newCookies' = getCookie headers'
    Tuple sessionOut' _ <- { request: TestRequest defaultRequest { headers = StrMap.singleton "cookie" newCookies' }
                          , response: TestResponse Nothing [] []
                          , components: { sessions: { key: "session", store: store' }
                                        , cookies: unit }}
                          # runMiddleware (cookies :*> getSession) >>> run
    sessionOut' `shouldEqual` Nothing

spec :: forall e. Spec (ref :: REF, console :: CONSOLE, random :: RANDOM, buffer :: BUFFER, crypto :: CRYPTO | e) Unit
spec = do
  describe "Hyper.Node.Session.InMemory" do
    testStore (liftEff newInMemorySessionStore) "value1" "value2"
  describe "Hyper.Node.Session.Cookie" do
    testStore (CookieStore <$> mkSecret) "value1" "value2"
