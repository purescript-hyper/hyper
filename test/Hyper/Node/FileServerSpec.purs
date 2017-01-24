module Hyper.Node.FileServerSpec where

import Prelude
import Node.Buffer as Buffer
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(Just))
import Data.Tuple (Tuple(Tuple))
import Hyper.Core (statusOK, try, fallbackTo, statusNotFound, writeStatus)
import Hyper.Node.FileServer (fileServer)
import Hyper.Node.Test (TestResponseBody(TestResponseBody))
import Hyper.Response (class Response, headers, respond)
import Hyper.Test.TestServer (testBody, TestResponse, testStatus, testServer, testHeaders, testResponseWriter)
import Node.Buffer (Buffer, BUFFER)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Test.Spec (it, Spec, describe)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.String (shouldContain)

serveFilesAndGet
  :: forall m e.
     (MonadAff (fs :: FS, buffer :: BUFFER | e) m, Response TestResponseBody m Buffer) =>
     String
  -> m (TestResponse TestResponseBody)
serveFilesAndGet path =
  { request: { url: path }
  , response: { writer: testResponseWriter }
  , components: {}
  }
  # app
  # testServer
  where
    app = fallbackTo notFound (try (fileServer "test/Hyper/Node/FileServerSpec"))

    notFound conn = do
      body <- liftEff (Buffer.fromString "Not Found" UTF8)
      writeStatus statusNotFound conn
        >>= headers []
        >>= respond body

spec :: forall e. Spec (fs :: FS, buffer :: BUFFER | e) Unit
spec =
  describe "Hyper.Node.FileServer" do
    let assertBody assertion response expected =
          case testBody response of
            TestResponseBody chunks -> do
              body <- liftEff (Buffer.concat chunks >>= Buffer.toString UTF8)
              body `assertion` expected
        bodyShouldEqual = assertBody shouldEqual
        bodyShouldContain = assertBody shouldContain

    describe "fileServer" do

      it "serves the exactly matched file" do
        response <- serveFilesAndGet "some-file.txt"
        testStatus response `shouldEqual` Just statusOK
        -- TODO: Figure out that this is text/plain
        testHeaders response `shouldEqual` [ Tuple "Content-Type" "*/*; charset=utf-8"
                                           , Tuple "Content-Length" "75"
                                           ]
        response `bodyShouldEqual` "λ Some file contents, med sådana tecken vi använder i Sverige! 🇸🇪\n"

      it "returns Nothing when no file matches the request" do
        response <- serveFilesAndGet "some-file-that-does-not-exist.txt"
        testStatus response `shouldEqual` Just statusNotFound
        response `bodyShouldEqual` "Not Found"

      it "serves index.html, if exists, when matching directory" do
        response <- serveFilesAndGet "/"
        testStatus response `shouldEqual` Just statusOK
        -- TODO: Figure out that this is text/html
        testHeaders response `shouldEqual` [ Tuple "Content-Type" "*/*; charset=utf-8"
                                           , Tuple "Content-Length" "144"
                                           ]
        response `bodyShouldContain` "<h1>Test File</h1>"
