module Hyper.Node.FileServerSpec where

import Prelude
import Node.Buffer as Buffer
import Control.IxMonad (ibind)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Hyper.Middleware (evalMiddleware)
import Hyper.Node.FileServer (fileServer)
import Hyper.Node.Test (TestResponseBody(TestResponseBody))
import Hyper.Response (ResponseEnded, headers, respond, writeStatus)
import Hyper.Status (statusNotFound, statusOK)
import Hyper.Test.TestServer (TestRequest(..), TestResponse(..), defaultRequest, testBody, testHeaders, testServer, testStatus)
import Node.Buffer (BUFFER)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Test.Spec (it, Spec, describe)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.String (shouldContain)

serveFilesAndGet
  :: forall m e.
     (MonadAff (fs :: FS, buffer :: BUFFER | e) m) =>
     String
  -> m (TestResponse TestResponseBody ResponseEnded)
serveFilesAndGet path =
  { request: TestRequest (defaultRequest { url = path })
  , response: TestResponse Nothing [] []
  , components: {}
  }
  # evalMiddleware app
  # testServer
  where
    app = fileServer "test/Hyper/Node/FileServerSpec" on404 []

    on404 = do
      body <- liftEff (Buffer.fromString "Not Found" UTF8)
      _ <- writeStatus statusNotFound
      _ <- headers []
      respond body
      where bind = ibind

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
