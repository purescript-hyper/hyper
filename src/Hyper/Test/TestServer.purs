module Hyper.Test.TestServer where

import Control.Alt ((<|>))
import Control.Applicative (pure, (*>))
import Control.Monad (class Monad, void)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.Function ((<<<))
import Data.Maybe (Maybe(Nothing, Just))
import Data.Monoid (class Monoid)
import Data.Semigroup (class Semigroup, (<>))
import Hyper.Core (StatusLineOpen(StatusLineOpen), HeadersOpen(HeadersOpen), Status, class ResponseWriter, Header, HeadersClosed(HeadersClosed), ResponseEnded(ResponseEnded), Conn)

data TestResponse = TestResponse (Maybe Status) (Array Header) String

testStatus ∷ TestResponse → Maybe Status
testStatus (TestResponse status _ _) = status

testHeaders ∷ TestResponse → Array Header
testHeaders (TestResponse _ headers _) = headers

testBody ∷ TestResponse → String
testBody (TestResponse _ _ body) = body

instance semigroupTestResponse ∷ Semigroup TestResponse where
  append (TestResponse status headers body) (TestResponse status' headers' body') =
    TestResponse (status <|> status') (headers <> headers') (body <> body')

instance monoidTestResponse ∷ Monoid TestResponse where
  mempty = TestResponse Nothing [] ""

testServer ∷ ∀ m a. Monad m ⇒ WriterT TestResponse m a → m TestResponse
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

instance responseWriterTestResponseWriter :: Monad m =>
                                             ResponseWriter TestResponseWriter (WriterT TestResponse m) where
  writeStatus status conn =
    tell (TestResponse (Just status) [] "") *> pure (withState HeadersOpen conn)

  writeHeader header conn =
    tell (TestResponse Nothing [header] "") *> pure conn

  closeHeaders = pure <<< withState HeadersClosed

  send s conn =
    tell (TestResponse Nothing [] s) *> pure conn

  end = pure <<< withState ResponseEnded
