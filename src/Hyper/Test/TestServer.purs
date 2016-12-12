module Hyper.Test.TestServer where

import Control.Applicative (pure, (*>))
import Control.Monad (class Monad, void)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.Function ((<<<))
import Data.Monoid (class Monoid)
import Data.Semigroup (class Semigroup, (<>))
import Hyper.Core (class ResponseWriter, Header, HeadersClosed(HeadersClosed), ResponseEnded(ResponseEnded), Conn)

data TestResponse = TestResponse (Array Header) String

testHeaders ∷ TestResponse → Array Header
testHeaders (TestResponse headers _) = headers

testBody ∷ TestResponse → String
testBody (TestResponse _ body) = body

instance semigroupTestResponse ∷ Semigroup TestResponse where
  append (TestResponse headers body) (TestResponse headers' body') =
    TestResponse (headers <> headers') (body <> body')

instance monoidTestResponse ∷ Monoid TestResponse where
  mempty = TestResponse [] ""

testServer ∷ ∀ m a. Monad m ⇒ WriterT TestResponse m a → m TestResponse
testServer = execWriterT <<< void

data TestResponseWriter state = TestResponseWriter state

withState ∷ ∀ req res c a b.
            b 
          → Conn req { writer ∷ TestResponseWriter a | res } c
          → Conn req { writer ∷ TestResponseWriter b | res } c
withState s conn = 
  case conn.response.writer of
       TestResponseWriter _ → conn { response = (conn.response { writer = TestResponseWriter s }) }

instance responseWriterTestResponseWriter :: Monad m =>
                                             ResponseWriter TestResponseWriter (WriterT TestResponse m) where
  writeHeader header conn =
    tell (TestResponse [header] "") *> pure conn
    
  closeHeaders = pure <<< withState HeadersClosed
    
  send s conn =
    tell (TestResponse [] s) *> pure conn

  end = pure <<< withState ResponseEnded
