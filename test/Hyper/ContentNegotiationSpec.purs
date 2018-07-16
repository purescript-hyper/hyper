module Hyper.ContentNegotiationSpec where

import Prelude
import Data.Map as Map
import Data.Either (Either(..))
import Data.List (fromFoldable)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationJSON, textHTML)
import Data.NonEmpty (NonEmpty(..))
import Data.Tuple (Tuple(..))
import Hyper.ContentNegotiation (AcceptEntry(..), AcceptHeader(..), AcceptParams(..), MediaRange(..), MediaRangeType(..), NegotiationResult(..), negotiateContent, parseAcceptHeader)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Hyper.ContentNegotiation" do
    describe "parseAcceptHeader" $
      it "parses multiple accept entries" $
        parseAcceptHeader "audio/*; q=0.2, audio/basic"
          `shouldEqual`
          Right (AcceptHeader [ AcceptEntry
                                (MediaRange (Literal "audio") Wildcard)
                                (AcceptParams 1.0 (Map.singleton "q" "0.2"))
                              , AcceptEntry
                                (MediaRange (Literal "audio") (Literal "basic"))
                                (AcceptParams 1.0 Map.empty)
                              ])

    describe "negotiateContent" do
      it "returns first without accept header" do
        negotiateContent Nothing (responses jsonResponse [])
          `shouldEqual`
          Default jsonResponse

      it "returns NotAcceptable with empty accept header" do
        negotiateContent (Just (AcceptHeader [])) (responses jsonResponse [])
          `shouldEqual`
          NotAcceptable (AcceptHeader [])

      it "returns only matching in accept header with single response" do
        let accept = Just (AcceptHeader [ AcceptEntry
                                          (MediaRange (Literal "application") (Literal "json"))
                                          (AcceptParams 1.0 Map.empty)
                                        ])
        negotiateContent accept (responses jsonResponse [])
          `shouldEqual`
          Match jsonResponse

      it "returns only matching in accept header with multiple responses" do
        let accept = Just (AcceptHeader [ AcceptEntry
                                          (MediaRange (Literal "application") (Literal "json"))
                                          (AcceptParams 1.0 Map.empty)
                                        ])
        negotiateContent accept (responses jsonResponse [htmlResponse])
          `shouldEqual`
          Match jsonResponse

      it "returns only matching in accept header with multiple responses regardless of order" do
        let accept = Just (AcceptHeader [ AcceptEntry
                                          (MediaRange (Literal "application") (Literal "json"))
                                          (AcceptParams 1.0 Map.empty)
                                        ])
        negotiateContent accept (responses htmlResponse [jsonResponse])
          `shouldEqual`
          Match jsonResponse

      it "returns based on higher q value with multiple responses" do
        let accept = Just (AcceptHeader [ AcceptEntry
                                          (MediaRange (Literal "application") (Literal "json"))
                                          (AcceptParams 0.5 Map.empty)
                                        , AcceptEntry
                                          (MediaRange (Literal "text") (Literal "html"))
                                          (AcceptParams 0.7 Map.empty)
                                        ])
        negotiateContent accept (responses htmlResponse [jsonResponse])
          `shouldEqual`
          Match htmlResponse

      pending "returns based on specificity when equal q values"

    where
      responses x xs =
        NonEmptyList (NonEmpty x (fromFoldable xs))

      jsonResponse = Tuple applicationJSON "{}"
      htmlResponse = Tuple textHTML "<br/>"
