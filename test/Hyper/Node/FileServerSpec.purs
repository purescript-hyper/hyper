module Hyper.Node.FileServerSpec where

import Prelude
import Control.Monad.Aff.Class (class MonadAff)
import Data.Maybe (Maybe(Just))
import Data.Tuple (Tuple(Tuple))
import Hyper.Core (statusOK, try, fallbackTo, statusNotFound, writeStatus)
import Hyper.Node.FileServer (fileServer)
import Hyper.Response (headers, respond)
import Hyper.Test.TestServer (TestResponse, testStatus, testServer, testBody, testHeaders, testResponseWriter)
import Node.Buffer (BUFFER)
import Node.FS (FS)
import Test.Spec (it, Spec, describe)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.String (shouldContain)

serveFilesAndGet
  :: forall m e.
     MonadAff ( fs :: FS, buffer :: BUFFER | e) m =>
     String
  -> m TestResponse
serveFilesAndGet path =
  { request: { url: path }
  , response: { writer: testResponseWriter }
  , components: {}
  }
  # app
  # testServer
  where
    app = fallbackTo notFound (try (fileServer "test/Hyper/Node/FileServerSpec"))

    notFound =
      writeStatus statusNotFound
      >=> headers []
      >=> respond "Not Found"


spec :: forall e. Spec (fs :: FS, buffer :: BUFFER | e) Unit
spec =
  describe "Hyper.Node.FileServer" do

    describe "fileServer" do
      it "serves the exactly matched file" do
        response <- serveFilesAndGet "some-file.txt"
        testStatus response `shouldEqual` Just statusOK
        -- TODO: Figure out that this is text/plain
        testHeaders response `shouldEqual` [ Tuple "Content-Type" "*/*; charset=utf-8"
                                           , Tuple "Content-Length" "75"
                                           ]
        testBody response `shouldEqual` "λ Some file contents, med sådana tecken vi använder i Sverige! 🇸🇪\n"

      it "returns Nothing when no file matches the request" do
        response <- serveFilesAndGet "some-file-that-does-not-exist.txt"
        testStatus response `shouldEqual` Just statusNotFound
        testBody response `shouldEqual` "Not Found"

      it "serves index.html, if exists, when matching directory" do
        response <- serveFilesAndGet "/"
        testStatus response `shouldEqual` Just statusOK
        -- TODO: Figure out that this is text/html
        testHeaders response `shouldEqual` [ Tuple "Content-Type" "*/*; charset=utf-8"
                                           , Tuple "Content-Length" "144"
                                           ]
        testBody response `shouldContain` "<h1>Test File</h1>"
