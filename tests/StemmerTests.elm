module StemmerTests exposing (StemCase, stemmingFixture, testStemmer, tests)

import Expect
import FrenchStemmer
import Test exposing (..)


{-| Test Stemmer, many many less tests than StemmerTestsFullPorter.elm..
So runs must faster.
-}
type alias StemCase =
    ( String, String )


tests : Test
tests =
    describe "Stemmer Tokenizer tests" <|
        List.map testStemmer stemmingFixture


testStemmer : StemCase -> Test
testStemmer ( word, expectedStem ) =
    test ("stem " ++ word ++ " to " ++ expectedStem ++ " ") <|
        \() -> Expect.equal expectedStem (FrenchStemmer.stemmer True word)


stemmingFixture : List StemCase
stemmingFixture =
    List.map2 Tuple.pair words stems


words =
    [ "continu"
    , "continua"
    , "continuait"
    , "continuant"
    , "continuation"
    , "continue"
    , "continué"
    , "continuel"
    , "continuelle"
    , "continuellement"
    , "continuelles"
    , "continuels"
    , "continuer"
    , "continuera"
    , "continuerait"
    , "continueront"
    , "continuez"
    , "continuité"
    , "continuons"
    , "contorsions"
    , "contour"
    , "contournait"
    , "contournant"
    , "contourne"
    , "contours"
    , "contractait"
    , "contracté"
    , "contractée"
    , "contracter"
    , "contractés"
    , "contractions"
    , "contradictoirement"
    , "contradictoires"
    , "contraindre"
    , "contraint"
    , "contrainte"
    , "contraintes"
    , "contraire"
    , "contraires"
    , "contraria"
    , "main"
    , "mains"
    , "maintenaient"
    , "maintenait"
    , "maintenant"
    , "maintenir"
    , "maintenue"
    , "maintien"
    , "maintint"
    , "maire"
    , "maires"
    , "mairie"
    , "mais"
    , "maïs"
    , "maison"
    , "maisons"
    , "maistre"
    , "maitre"
    , "maître"
    , "maîtres"
    , "maîtresse"
    , "maîtresses"
    , "majesté"
    , "majestueuse"
    , "majestueusement"
    , "majestueux"
    , "majeur"
    , "majeure"
    , "major"
    , "majordome"
    , "majordomes"
    , "majorité"
    , "majorités"
    , "mal"
    , "malacca"
    , "malade"
    , "malades"
    , "maladie"
    , "maladies"
    , "maladive"
    ]


stems =
    [ "continu"
    , "continu"
    , "continu"
    , "continu"
    , "continu"
    , "continu"
    , "continu"
    , "continuel"
    , "continuel"
    , "continuel"
    , "continuel"
    , "continuel"
    , "continu"
    , "continu"
    , "continu"
    , "continu"
    , "continu"
    , "continu"
    , "continuon"
    , "contors"
    , "contour"
    , "contourn"
    , "contourn"
    , "contourn"
    , "contour"
    , "contract"
    , "contract"
    , "contract"
    , "contract"
    , "contract"
    , "contract"
    , "contradictoir"
    , "contradictoir"
    , "contraindr"
    , "contraint"
    , "contraint"
    , "contraint"
    , "contrair"
    , "contrair"
    , "contrari"
    , "main"
    , "main"
    , "mainten"
    , "mainten"
    , "mainten"
    , "mainten"
    , "maintenu"
    , "maintien"
    , "maintint"
    , "mair"
    , "mair"
    , "mair"
    , "mais"
    , "maï"
    , "maison"
    , "maison"
    , "maistr"
    , "maitr"
    , "maîtr"
    , "maîtr"
    , "maîtress"
    , "maîtress"
    , "majest"
    , "majestu"
    , "majestu"
    , "majestu"
    , "majeur"
    , "majeur"
    , "major"
    , "majordom"
    , "majordom"
    , "major"
    , "major"
    , "mal"
    , "malacc"
    , "malad"
    , "malad"
    , "malad"
    , "malad"
    , "malad"
    ]



-------------------------------------------------------------------------------
