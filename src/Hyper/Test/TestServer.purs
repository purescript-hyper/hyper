module Hyper.Test.TestServer where

import Control.Alt ((<|>))
import Control.Applicative (pure, (*>))
import Control.Monad (class Monad, void)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.Foldable (fold)
import Data.Function ((<<<))
import Data.Maybe (Maybe(Nothing, Just))
import Data.Monoid (mempty, class Monoid)
import Data.Semigroup (class Semigroup, (<>))
import Hyper.Core (StatusLineOpen(StatusLineOpen), HeadersOpen(HeadersOpen), Status, class ResponseWriter, Header, BodyOpen(BodyOpen), ResponseEnded(ResponseEnded), Conn)

data TestResponse b = TestResponse (Maybe Status) (Array Header) (Array b)

testStatus :: forall b. TestResponse b → Maybe Status
testStatus (TestResponse status _ _) = status

testHeaders :: forall b. TestResponse b → Array Header
testHeaders (TestResponse _ headers _) = headers

testBodyChunks :: forall b. TestResponse b → Array b
testBodyChunks (TestResponse _ _ body) = body

testBody :: forall b. Monoid b => TestResponse b → b
testBody (TestResponse _ _ body) = fold body

instance semigroupTestResponse :: Semigroup (TestResponse b) where
  append (TestResponse status headers bodyChunks) (TestResponse status' headers' bodyChunks') =
    TestResponse (status <|> status') (headers <> headers') (bodyChunks <> bodyChunks')

instance monoidTestResponse :: Monoid (TestResponse b) where
  mempty = TestResponse Nothing [] []

testServer :: forall m a b. Monad m ⇒ WriterT (TestResponse b) m a → m (TestResponse b)
testServer = execWriterT <<< void

data TestResponseWriter state = TestResponseWriter state

testResponseWriter :: TestResponseWriter StatusLineOpen
testResponseWriter = TestResponseWriter StatusLineOpen

withState ∷ ∀ req res c a b.
            b
          → Conn req { writer ∷ TestResponseWriter a | res } c
          → Conn req { writer ∷ TestResponseWriter b | res } c
withState s conn =
  case conn.response.writer of
       TestResponseWriter _ → conn { response = (conn.response { writer = TestResponseWriter s }) }

instance responseWriterTestResponseWriter :: (Monad m) =>
                                             ResponseWriter
                                             TestResponseWriter
                                             b
                                             (WriterT (TestResponse b) m) where
  writeStatus status conn =
    tell (TestResponse (Just status) [] []) *> pure (withState HeadersOpen conn)

  writeHeader header conn =
    tell (TestResponse Nothing [header] mempty) *> pure conn

  closeHeaders = pure <<< withState BodyOpen

  send chunk conn =
    tell (TestResponse Nothing [] [chunk]) *> pure conn

  end = pure <<< withState ResponseEnded
