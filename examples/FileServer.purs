module Examples.FileServer where

import Prelude

import Control.Monad.Indexed.Qualified as Ix
import Effect (Effect)
import Data.Tuple (Tuple(Tuple))
import Hyper.Node.FileServer (fileServer)
import Hyper.Node.Server (defaultOptionsWithLogging, runServer)
import Hyper.Response (headers, respond, writeStatus)
import Hyper.Status (statusNotFound)
import Node.Encoding (Encoding(UTF8))

main :: Effect Unit
main =
  let
    notFound = Ix.do
      writeStatus statusNotFound
      headers []
      respond (Tuple "<h1>Not Found</h1>" UTF8)
    app = fileServer "examples/FileServer" notFound
  in runServer defaultOptionsWithLogging {} app
