module Hyper.TestUtilities where

import Prelude
import Control.Monad.Eff.Class (liftEff, class MonadEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Tuple (Tuple(Tuple))
import Hyper.Core (ResponseEnded(ResponseEnded), HeadersClosed(HeadersClosed), class ResponseWriter)

data TestResponseWriter = TestResponseWriter

instance responseWriterTestResponseWriter :: MonadEff (console :: CONSOLE | e) m =>
                                             ResponseWriter TestResponseWriter m where
  writeHeader _ (Tuple name value) conn = do
    liftEff (log $ "Writing header: " <> name <> " with value: " <> value)
    pure conn
  closeHeaders _ { request, response, components } = do
    liftEff (log "Closing headers test.")
    pure { request: request
         , response: (response { state = HeadersClosed })
         , components: components
         }
  send writer s { request, response, components } = do
    liftEff (log $ "Sending: " <> s)
    pure { request: request
         , response: response
         , components: components
         }

  end writer { request, response, components } = do
    liftEff (log "Ending response in test.")
    pure { request: request
         , response: (response { state = ResponseEnded })
         , components: components
         }
