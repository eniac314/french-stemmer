module StemmerTestsFullPorter exposing (StemCase, expects, fixture, inputs, testStemmer, tests)

{-
   StemmerFixture one bombs compiling in Elm 0.18
   use alternate string form and split.

   List.map testStemmer StemmerFixture.fixture
-}

import Expect
import FrenchStemmer
import StemmerFixture2
import Test exposing (..)


inputs =
    String.split "," StemmerFixture2.inputString


expects =
    String.split "," StemmerFixture2.expectString


fixture =
    List.map2 Tuple.pair inputs expects


{-| Full set of porter stemmer test as converted from the porter stemmer
website from the voc.txt and output.txt files and put in StemmerFixture.elm.
-}
type alias StemCase =
    ( String, String )


tests : Test
tests =
    describe "Stemmer Tokenizer tests for Full Porter" <|
        List.map testStemmer fixture


testStemmer : StemCase -> Test
testStemmer ( word, expectedStem ) =
    test ("stem " ++ word ++ " to " ++ expectedStem ++ " ") <|
        \() -> Expect.equal expectedStem (FrenchStemmer.stemmer True word)
