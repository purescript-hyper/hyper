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

execTestServer ∷ ∀ m a. Functor m ⇒ WriterT TestResponse m a -> m TestResponse
execTestServer action = execWriterT action

testServer ∷ ∀ m a. Monad m ⇒ WriterT TestResponse m a → m TestResponse
testServer m = execTestServer (void m)

data TestResponseWriter = TestResponseWriter
  
instance responseWriterTestResponseWriter :: Monad m =>
                                             ResponseWriter TestResponseWriter (WriterT TestResponse m) where
  writeHeader _ header conn = do
    tell (TestResponse [header] "")
    pure conn
  closeHeaders _ { request, response, components } = do
    pure { request: request
         , response: (response { state = HeadersClosed })
         , components: components
         }
  send writer s { request, response, components } = do
    tell (TestResponse [] s)
    pure { request: request
         , response: response
         , components: components
         }

  end writer { request, response, components } = do
    pure { request: request
         , response: (response { state = ResponseEnded })
         , components: components
         }
