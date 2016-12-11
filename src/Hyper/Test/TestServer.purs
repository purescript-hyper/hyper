module Hyper.Test.TestServer where

import Prelude
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.Monoid (class Monoid)
import Hyper.Core (ResponseEnded(ResponseEnded), HeadersClosed(HeadersClosed), class ResponseWriter, Header)

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

data TestResponseWriter = TestResponseWriter
  
instance responseWriterTestResponseWriter :: Monad m =>
                                             ResponseWriter TestResponseWriter (WriterT TestResponse m) where
  writeHeader _ header conn =
    tell (TestResponse [header] "") *> pure conn
    
  closeHeaders _ conn =
    pure conn { response = (conn.response { state = HeadersClosed }) }
    
  send writer s conn =
    tell (TestResponse [] s) *> pure conn

  end writer conn =
    pure conn { response = (conn.response { state = ResponseEnded }) }
