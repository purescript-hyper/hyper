module Hyper.ContentNegotiationSpec where

import Prelude
import Data.Map as Map
import Data.Either (Either(..))
import Hyper.ContentNegotiation (AcceptEntry(..), AcceptHeader(..), AcceptParams(..), MediaRange(..), MediaRangeType(..), parseAcceptHeader)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: forall e. Spec e Unit
spec = do
  describe "Hyper.ContentNegotiation" $
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
