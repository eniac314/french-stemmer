module FrenchStemmer exposing
    ( debugStemmer
    , frenchStopWords
    , stemmer
    , step1
    , step2a
    , step2b
    , step3
    , step4
    , step5
    )

import List.Extra exposing (dropWhile, takeWhile)



-- source http://snowball.tartarus.org/algorithms/french/stemmer.html


accentedForms =
    [ 'â', 'à', 'ç', 'ë', 'é', 'ê', 'è', 'ï', 'î', 'ô', 'û', 'ù' ]


vowels =
    [ 'a', 'e', 'i', 'o', 'u', 'y', 'â', 'à', 'ë', 'é', 'ê', 'è', 'ï', 'î', 'ô', 'û', 'ù' ]


isVowel c =
    List.member c vowels


vowelMarked : String -> String
vowelMarked lwcWord =
    let
        p current left right =
            case ( current, left, right ) of
                ( c, l :: ls, r :: rs ) ->
                    if (c == 'u' || c == 'i') && (List.member l vowels && List.member r vowels) then
                        --u or i preceded and followed by a vowel
                        Char.toUpper c

                    else if (c == 'y') && (List.member l vowels || List.member r vowels) then
                        --y preceded or followed by a vowel
                        'Y'

                    else if (c == 'u') && (l == 'q') then
                        --u after q
                        'U'

                    else
                        c

                ( 'y', _, r :: rs ) ->
                    if List.member r vowels then
                        --y followed by a vowel
                        'Y'

                    else
                        'y'

                _ ->
                    current
    in
    changeCharIf p [] (String.toList lwcWord)
        |> String.fromList


rVFunc : String -> String
rVFunc i =
    if String.startsWith "par" i || String.startsWith "col" i || String.startsWith "tap" i then
        --par, col or tap, at the begining of a word is also taken to define RV as the region to their right
        String.dropLeft 3 i

    else if
        List.take 2 (String.toList i)
            |> List.map isVowel
            |> (\l -> l == [ True, True ])
    then
        --If the word begins with two vowels, RV is the region after the third letter
        String.dropLeft 3 i

    else
        --the region after the first vowel not at the beginning of the word
        String.dropLeft 1 i
            |> dropWhileString (not << isVowel)
            |> String.dropLeft 1


r1Func : String -> String
r1Func i =
    --R1 is the region after the first non-vowel following a vowel,
    --or the end of the word if there is no such non-vowel
    rN (String.toList i)
        |> String.fromList


r2Func : String -> String
r2Func i =
    -- R2 is the region after the first non-vowel following a vowel in R1, or the end of the word if there is no such non-vowel
    rN (String.toList i)
        |> rN
        |> String.fromList


type Step
    = Step1
    | Step2a
    | Step2b
    | Step3
    | Step4
    | Step5
    | Step6


type alias Record =
    { input : String
    , currentRV : String
    , currentR1 : String
    , currentR2 : String
    , realised : List Step
    , lastEffective : Maybe Step
    }



-------------------------------------------------------------------------------
------------
-- Step 1 --
------------


suffixes1 =
    [ "ance", "iqUe", "isme", "able", "iste", "eux", "ances", "iqUes", "ismes", "ables", "istes" ]


suffixes2 =
    [ "atrice", "ateur", "ation", "atrices", "ateurs", "ations" ]


suffixes3 =
    [ "logie", "logies" ]


suffixes4 =
    [ "usion", "ution", "usions", "utions" ]


suffixes5 =
    [ "ence", "ences" ]


suffixes6 =
    [ "ement", "ements" ]


suffixes7 =
    [ "ité", "ités" ]


suffixes8 =
    [ "if", "ive", "ifs", "ives" ]


suffixes9 =
    [ "eaux" ]


suffixes10 =
    [ "aux" ]


suffixes11 =
    [ "euse", "euses" ]


suffixes12 =
    [ "issement", "issements" ]


suffixes13 =
    [ "amment" ]


suffixes14 =
    [ "emment" ]


suffixes15 =
    [ "ment", "ments" ]


step1Suffixes =
    List.concat [ suffixes1, suffixes2, suffixes3, suffixes4, suffixes5, suffixes6, suffixes7, suffixes8, suffixes9, suffixes10, suffixes11, suffixes12, suffixes13, suffixes14, suffixes15 ]


step1 : Record -> Record
step1 rec =
    --Standard suffix removal
    (case findLongestIfPresent rec.input step1Suffixes of
        Just suffixe ->
            if List.member suffixe suffixes1 then
                --ance   iqUe   isme   able   iste   eux   ances   iqUes   ismes   ables   istes
                if String.contains suffixe rec.currentR2 then
                    --delete if in R2
                    replaceSuffixe suffixe "" rec.input

                else
                    rec.input

            else if List.member suffixe suffixes2 then
                --atrice   ateur   ation   atrices   ateurs   ations
                (if String.contains suffixe rec.currentR2 then
                    --delete if in R2
                    replaceSuffixe suffixe "" rec.input

                 else
                    rec.input
                )
                    |> (\res ->
                            --if preceded by ic
                            if String.endsWith "ic" res then
                                if String.endsWith ("ic" ++ suffixe) rec.currentR2 then
                                    --delete if in R2
                                    replaceSuffixe "ic" "" res

                                else
                                    --else replace by iqU
                                    replaceSuffixe "ic" "iqU" res

                            else
                                res
                       )

            else if List.member suffixe suffixes3 then
                --logie   logies
                if String.contains suffixe rec.currentR2 then
                    --replace with log if in R2
                    replaceSuffixe suffixe "log" rec.input

                else
                    rec.input

            else if List.member suffixe suffixes4 then
                --usion   ution   usions   utions
                if String.contains suffixe rec.currentR2 then
                    --replace with u if in R2
                    replaceSuffixe suffixe "u" rec.input

                else
                    rec.input

            else if List.member suffixe suffixes5 then
                --ence   ences
                if String.contains suffixe rec.currentR2 then
                    --replace with ent if in R2
                    replaceSuffixe suffixe "ent" rec.input

                else
                    rec.input

            else if List.member suffixe suffixes6 then
                --ement   ements
                (if String.contains suffixe rec.currentRV then
                    --delete if in RV
                    replaceSuffixe suffixe "" rec.input

                 else
                    rec.input
                )
                    |> (\res ->
                            if String.endsWith ("ativ" ++ suffixe) rec.currentR2 then
                                --if further preceded by at, delete if in R2
                                replaceSuffixe "ativ" "" res

                            else if String.endsWith ("iv" ++ suffixe) rec.currentR2 then
                                --if preceded by iv, delete if in R2
                                replaceSuffixe "iv" "" res

                            else if String.endsWith ("eus" ++ suffixe) rec.currentR2 then
                                --if preceded by eus, delete if in R2
                                replaceSuffixe "eus" "" res

                            else if String.endsWith ("eus" ++ suffixe) rec.currentR1 then
                                --else replace by eux if in R1
                                replaceSuffixe "eus" "eux" res

                            else if String.endsWith ("abl" ++ suffixe) rec.currentR2 || String.endsWith ("iqU" ++ suffixe) rec.currentR2 then
                                --if preceded by abl or iqU, delete if in R2
                                String.dropRight 3 res

                            else if String.endsWith ("ièr" ++ suffixe) rec.currentRV || String.endsWith ("Ièr" ++ suffixe) rec.currentRV then
                                --if preceded by ièr or Ièr, replace by i if in RV
                                String.dropRight 3 res ++ "i"

                            else
                                res
                       )

            else if List.member suffixe suffixes7 then
                --ité   ités
                (if String.contains suffixe rec.currentR2 then
                    --delete if in R2
                    replaceSuffixe suffixe "" rec.input

                 else
                    rec.input
                )
                    |> (\res ->
                            if String.endsWith "abil" res then
                                --if preceded by abil
                                if String.endsWith ("abil" ++ suffixe) rec.currentR2 then
                                    --delete if in R2
                                    replaceSuffixe "abil" "" res

                                else
                                    --else replace by abl
                                    replaceSuffixe "abil" "abl" res

                            else if String.endsWith "ic" res then
                                --if preceded by ic
                                if String.endsWith ("ic" ++ suffixe) rec.currentR2 then
                                    --delete if in R2
                                    replaceSuffixe "ic" "" res

                                else
                                    --replace by iqU
                                    replaceSuffixe "ic" "iqU" res

                            else if String.endsWith ("iv" ++ suffixe) rec.currentR2 then
                                --if preceded by iv, delete if in R2
                                replaceSuffixe "iv" "" res

                            else
                                res
                       )

            else if List.member suffixe suffixes8 then
                --if   ive   ifs   ives
                (if String.contains suffixe rec.currentR2 then
                    --delete if in R2
                    replaceSuffixe suffixe "" rec.input

                 else
                    rec.input
                )
                    |> (\res ->
                            if String.endsWith "icat" res then
                                -- if further preceded by ic
                                if String.endsWith ("icat" ++ suffixe) rec.currentR2 then
                                    --delete if in R2
                                    replaceSuffixe "icat" "" res

                                else
                                    --else replace by iqU
                                    replaceSuffixe "icat" "iqU" res

                            else if String.endsWith ("at" ++ suffixe) rec.currentR2 then
                                --if preceded by at, delete if in R2
                                replaceSuffixe "at" "" res

                            else
                                res
                       )

            else if List.member suffixe suffixes9 then
                --eaux
                --replace with eau
                replaceSuffixe suffixe "eau" rec.input

            else if List.member suffixe suffixes10 then
                --aux
                if String.contains suffixe rec.currentR1 then
                    --replace with al if in R1
                    replaceSuffixe suffixe "al" rec.input

                else
                    rec.input

            else if List.member suffixe suffixes11 then
                --euse   euses
                if String.contains suffixe rec.currentR2 then
                    --delete if in R2
                    replaceSuffixe suffixe "" rec.input

                else if String.contains suffixe rec.currentR1 then
                    --else replace by eux if in R1
                    replaceSuffixe suffixe "eux" rec.input

                else
                    rec.input

            else if List.member suffixe suffixes12 then
                --issement   issements
                if String.contains suffixe rec.currentR1 && precededByCharMatchingP rec.input suffixe (\c -> not <| isVowel c) then
                    --delete if in R1 and preceded by a non-vowel
                    replaceSuffixe suffixe "" rec.input

                else
                    rec.input

            else if List.member suffixe suffixes13 then
                --amment
                if String.contains suffixe rec.currentRV then
                    --replace with ant if in RV
                    replaceSuffixe suffixe "ant" rec.input

                else
                    rec.input

            else if List.member suffixe suffixes14 then
                --emment
                if String.contains suffixe rec.currentRV then
                    --replace with ant if in RV
                    replaceSuffixe suffixe "ent" rec.input

                else
                    rec.input

            else if List.member suffixe suffixes15 then
                --ment   ments
                if String.contains suffixe rec.currentRV && precededByCharMatchingP rec.currentRV suffixe (\c -> isVowel c) then
                    --delete if preceded by a vowel in RV
                    replaceSuffixe suffixe "" rec.input
                    --|> String.dropRight 1

                else
                    rec.input

            else
                rec.input

        Nothing ->
            rec.input
    )
        |> (\newInput ->
                { input = newInput
                , currentRV = rVFunc newInput
                , currentR1 = r1Func newInput
                , currentR2 = r2Func newInput
                , realised = [ Step1 ]
                , lastEffective =
                    if newInput /= rec.input then
                        Just Step1

                    else
                        rec.lastEffective
                }
           )



-------------------------------------------------------------------------------
------------
-- Step2a --
------------
--Verb suffixes beginning i


iVerbSuffixes =
    [ "îmes"
    , "ît"
    , "îtes"
    , "i"
    , "ie"
    , "ies"
    , "ir"
    , "ira"
    , "irai"
    , "iraIent"
    , "irais"
    , "irait"
    , "iras"
    , "irent"
    , "irez"
    , "iriez"
    , "irions"
    , "irons"
    , "iront"
    , "is"
    , "issaIent"
    , "issais"
    , "issait"
    , "issant"
    , "issante"
    , "issantes"
    , "issants"
    , "isse"
    , "issent"
    , "isses"
    , "issez"
    , "issiez"
    , "issions"
    , "issons"
    , "it"
    ]


step2a : Record -> Record
step2a rec =
    (case findLongestIfPresent rec.currentRV iVerbSuffixes of
        Just suffixe ->
            if precededByCharMatchingP rec.input suffixe (\c -> (not <| isVowel c) && String.contains (String.cons c suffixe) rec.currentRV) then
                --Search for the longest among the following suffixes and if found, delete if preceded by a non-vowel.
                --Note that the non-vowel itself must also be in RV
                replaceSuffixe suffixe "" rec.input

            else
                rec.input

        Nothing ->
            rec.input
    )
        |> (\newInput ->
                { input = newInput
                , currentRV = rVFunc newInput
                , currentR1 = r1Func newInput
                , currentR2 = r2Func newInput
                , realised = Step2a :: rec.realised
                , lastEffective =
                    if newInput /= rec.input then
                        Just Step2a

                    else
                        rec.lastEffective
                }
           )



-------------------------------------------------------------------------------
------------
-- Step2b --
------------
--Other verb suffixes


otherVerbSuffixes1 =
    [ "ions" ]


otherVerbSuffixes2 =
    [ "é", "ée", "ées", "és", "èrent", "er", "era", "erai", "eraIent", "erais", "erait", "eras", "erez", "eriez", "erions", "erons", "eront", "ez", "iez" ]


otherVerbSuffixes3 =
    [ "âmes", "ât", "âtes", "a", "ai", "aIent", "ais", "ait", "ant", "ante", "antes", "ants", "as", "asse", "assent", "asses", "assiez", "assions" ]


otherVerbSuffixes =
    List.concat [ otherVerbSuffixes1, otherVerbSuffixes2, otherVerbSuffixes3 ]


step2b : Record -> Record
step2b rec =
    (case findLongestIfPresent rec.currentRV otherVerbSuffixes of
        Just suffixe ->
            if List.member suffixe otherVerbSuffixes1 then
                if String.contains suffixe rec.currentR2 then
                    --delete if in R2
                    replaceSuffixe suffixe "" rec.input

                else
                    rec.input

            else if List.member suffixe otherVerbSuffixes2 then
                --delete
                replaceSuffixe suffixe "" rec.input

            else if List.member suffixe otherVerbSuffixes3 then
                --delete
                replaceSuffixe suffixe "" rec.input
                    |> (\res ->
                            --if preceded by e, delete
                            --Note that the e that may be deleted in this last step must also be in RV
                            if String.endsWith "e" res then
                                if String.endsWith ("e" ++ suffixe) rec.currentRV then
                                    replaceSuffixe "e" "" res

                                else
                                    res

                            else
                                res
                       )

            else
                rec.input

        Nothing ->
            rec.input
    )
        |> (\newInput ->
                { input = newInput
                , currentRV = rVFunc newInput
                , currentR1 = r1Func newInput
                , currentR2 = r2Func newInput
                , realised = Step2b :: rec.realised
                , lastEffective =
                    if newInput /= rec.input then
                        Just Step2b

                    else
                        rec.lastEffective
                }
           )



-------------------------------------------------------------------------------
-----------
-- Step3 --
-----------


step3 rec =
    (case String.right 1 rec.input of
        "Y" ->
            String.dropRight 1 rec.input ++ "i"

        "ç" ->
            String.dropRight 1 rec.input ++ "c"

        _ ->
            rec.input
    )
        |> (\newInput ->
                { input = newInput
                , currentRV = rVFunc newInput
                , currentR1 = r1Func newInput
                , currentR2 = r2Func newInput
                , realised = Step3 :: rec.realised
                , lastEffective =
                    if newInput /= rec.input then
                        Just Step3

                    else
                        rec.lastEffective
                }
           )



-------------------------------------------------------------------------------
-----------
-- Step4 --
-----------


residualSuffixes1 =
    [ "ion" ]


residualSuffixes2 =
    [ "ier", "ière", "Ier", "Ière" ]


residualSuffixes3 =
    [ "e" ]


residualSuffixes4 =
    [ "ë" ]


residualSuffixes =
    List.concat [ residualSuffixes1, residualSuffixes2, residualSuffixes3, residualSuffixes4 ]


step4 : Record -> Record
step4 rec =
    -- Residual suffix
    (case List.reverse (String.toList rec.input) of
        's' :: c :: xs ->
            if not <| List.member c [ 'a', 'i', 'o', 'u', 'è', 's' ] then
                -- If the word ends with s, not preceded by a, i, o, u, è or s, delete it.
                { rec
                    | input = String.dropRight 1 rec.input
                    , currentRV = String.dropRight 1 rec.currentRV
                    , currentR1 = String.dropRight 2 rec.currentR1
                    , currentR2 = String.dropRight 1 rec.currentR2
                }

            else
                rec

        _ ->
            rec
    )
        |> (\rec_ ->
                (case findLongestIfPresent rec_.currentRV residualSuffixes of
                    --In the rest of step 4, all tests are confined to the RV region
                    Just suffixe ->
                        if List.member suffixe residualSuffixes1 then
                            if String.contains suffixe rec_.currentR2 && (precededBy rec_.currentRV suffixe "s" || precededBy rec_.currentRV suffixe "t") then
                                --delete if in R2 and preceded by s or t
                                replaceSuffixe suffixe "" rec_.input

                            else
                                rec_.input

                        else if List.member suffixe residualSuffixes2 then
                            --replace with i
                            replaceSuffixe suffixe "i" rec_.input

                        else if List.member suffixe residualSuffixes3 then
                            --delete
                            replaceSuffixe suffixe "" rec_.input

                        else if List.member suffixe residualSuffixes4 && precededBy rec_.currentRV suffixe "gu" then
                            --if preceded by gu, delete
                            replaceSuffixe suffixe "" rec_.input

                        else
                            rec_.input

                    Nothing ->
                        rec_.input
                )
                    |> (\newInput ->
                            { input = newInput
                            , currentRV = rVFunc newInput
                            , currentR1 = r1Func newInput
                            , currentR2 = r2Func newInput
                            , realised = Step4 :: rec.realised
                            , lastEffective =
                                if newInput /= rec.input then
                                    Just Step4

                                else
                                    rec.lastEffective
                            }
                       )
           )



-------------------------------------------------------------------------------
-----------
-- Step5 --
-----------


doubleEndings =
    [ "enn", "onn", "ett", "ell", "eill" ]


step5 : Record -> Record
step5 rec =
    --If the word ends enn, onn, ett, ell or eill, delete the last letter
    (case List.filter (\e -> String.endsWith e rec.input) doubleEndings of
        x :: xs ->
            String.dropRight 1 rec.input

        [] ->
            rec.input
    )
        |> (\newInput ->
                { input = newInput
                , currentRV = rVFunc newInput
                , currentR1 = r1Func newInput
                , currentR2 = r2Func newInput
                , realised = Step5 :: rec.realised
                , lastEffective =
                    if newInput /= rec.input then
                        Just Step5

                    else
                        rec.lastEffective
                }
           )



-------------------------------------------------------------------------------
-----------
-- Step6 --
-----------


step6 : Record -> Record
step6 rec =
    let
        ending =
            takeWhileString (\c -> c /= 'é' && c /= 'è') (String.reverse rec.input)
                |> String.reverse
    in
    (if ending == rec.input || ending == "" then
        rec.input

     else if String.all (not << isVowel) ending then
        String.dropRight (String.length ending + 1) rec.input
            ++ "e"
            ++ ending

     else
        rec.input
    )
        |> (\newInput ->
                { input = newInput
                , currentRV = rVFunc newInput
                , currentR1 = r1Func newInput
                , currentR2 = r2Func newInput
                , realised = Step6 :: rec.realised
                , lastEffective =
                    if newInput /= rec.input then
                        Just Step6

                    else
                        rec.lastEffective
                }
           )



-------------------------------------------------------------------------------
-------------
-- Stemmer --
-------------


stemmer : Bool -> String -> String
stemmer remAp word =
    let
        vowelMarkedStr =
            if remAp then
                removeApostrophePrefixes word

            else
                word
                    |> String.toLower
                    |> vowelMarked
    in
    stemmer_
        { input = vowelMarkedStr
        , currentRV = rVFunc vowelMarkedStr
        , currentR1 = r1Func vowelMarkedStr
        , currentR2 = r2Func vowelMarkedStr
        , realised = []
        , lastEffective = Nothing
        }
        |> .input


debugStemmer : String -> Record
debugStemmer word =
    let
        vowelMarkedStr =
            vowelMarked (String.toLower word)
    in
    stemmer_
        { input = vowelMarkedStr
        , currentRV = rVFunc vowelMarkedStr
        , currentR1 = r1Func vowelMarkedStr
        , currentR2 = r2Func vowelMarkedStr
        , realised = []
        , lastEffective = Nothing
        }


stemmer_ : Record -> Record
stemmer_ rec =
    step1 rec
        |> (\rec1 ->
                let
                    specialCase =
                        findLongestIfPresent rec.input step1Suffixes
                            |> Maybe.map (\s -> List.member s [ "amment", "emment", "ment", "ments" ])
                            |> Maybe.withDefault False
                in
                if rec1.input == rec.input || specialCase then
                    --Do step 2a if either no ending was removed by step 1, or if one of endings amment, emment, ment, ments was found.
                    step2a rec1
                        |> (\rec2a ->
                                --Do step 2b if step 2a was done, but failed to remove a suffix.
                                if rec2a.lastEffective /= Just Step2a then
                                    --step2a did nothing
                                    step2b rec2a

                                else
                                    rec2a
                           )
                        |> (\rec2 ->
                                --If the last step to be obeyed — either step 1, 2a or 2b — altered the word, do step 3
                                --Alternatively, if the last step to be obeyed did not alter the word, do step 4
                                if rec2.lastEffective /= Nothing then
                                    step3 rec2

                                else
                                    step4 rec2
                           )

                else if rec1.input /= rec.input then
                    --step1 did something
                    step3 rec1

                else
                    --step1 did nothing
                    step4 rec1
           )
        |> step5
        |> step6
        |> (\finalRec -> { finalRec | input = String.toLower finalRec.input })


removeApostrophePrefixes : String -> String
removeApostrophePrefixes =
    let
        toRemove =
            [ "l'", "d'", "qu'", "s'", "m'", "n'", "t'", "c'" ]

        remover remainingPrefixes word =
            case remainingPrefixes of
                [] ->
                    word

                p :: ps ->
                    if String.startsWith p word then
                        String.dropLeft (String.length p) word

                    else
                        remover ps word
    in
    remover toRemove



-------------------------------------------------------------------------------
----------------------
-- Helper functions --
----------------------


rN : List Char -> List Char
rN remaining =
    case remaining of
        c1 :: c2 :: xs ->
            if isVowel c1 && (not <| isVowel c2) then
                xs

            else
                rN (c2 :: xs)

        _ ->
            []


takeWhileString p s =
    String.toList s
        |> List.Extra.takeWhile p
        |> String.fromList


dropWhileString p s =
    String.toList s
        |> List.Extra.dropWhile p
        |> String.fromList


replaceSuffixe s r w =
    String.slice 0 (String.length w - String.length s) w ++ r


findLongestIfPresent word suffixes =
    List.filter (\s -> String.endsWith s word) suffixes
        |> List.sortBy String.length
        |> List.reverse
        |> List.head


precededBy word suff s =
    String.dropRight (String.length suff) word
        |> String.right (String.length s)
        |> (\res -> res == s)


precededByCharMatchingP word suff p =
    String.dropRight (String.length suff) word
        |> String.reverse
        |> String.uncons
        |> Maybe.map (Tuple.mapFirst p)
        |> Maybe.map Tuple.first
        |> Maybe.withDefault False


changeCharIf : (Char -> List Char -> List Char -> Char) -> List Char -> List Char -> List Char
changeCharIf p left right =
    case right of
        [] ->
            List.reverse left

        current :: xs ->
            changeCharIf p (p current left xs :: left) xs


startingRecord w =
    let
        vowelMarkedStr =
            vowelMarked (String.toLower w)
    in
    { input = vowelMarkedStr
    , currentRV = rVFunc vowelMarkedStr
    , currentR1 = r1Func vowelMarkedStr
    , currentR2 = r2Func vowelMarkedStr
    , realised = []
    , lastEffective = Nothing
    }



-------------------------------------------------------------------------------
-- tests --


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


tester =
    List.map2
        (\w s ->
            let
                result =
                    debugStemmer w
            in
            if result.input /= s then
                Just result

            else
                Nothing
        )
        words
        stems
        |> List.filterMap identity



-------------------------------------------------------------------------------
---------------------
-- frenchStopWords --
---------------------
--https://github.com/stopwords-iso/stopwords-fr


frenchStopWords =
    [ "a"
    , "abord"
    , "absolument"
    , "afin"
    , "ah"
    , "ai"
    , "aie"
    , "aient"
    , "aies"
    , "ailleurs"
    , "ainsi"
    , "ait"
    , "allaient"
    , "allo"
    , "allons"
    , "allô"
    , "alors"
    , "anterieur"
    , "anterieure"
    , "anterieures"
    , "apres"
    , "après"
    , "as"
    , "assez"
    , "attendu"
    , "au"
    , "aucun"
    , "aucune"
    , "aucuns"
    , "aujourd"
    , "aujourd'hui"
    , "aupres"
    , "auquel"
    , "aura"
    , "aurai"
    , "auraient"
    , "aurais"
    , "aurait"
    , "auras"
    , "aurez"
    , "auriez"
    , "aurions"
    , "aurons"
    , "auront"
    , "aussi"
    , "autant"
    , "autre"
    , "autrefois"
    , "autrement"
    , "autres"
    , "autrui"
    , "aux"
    , "auxquelles"
    , "auxquels"
    , "avaient"
    , "avais"
    , "avait"
    , "avant"
    , "avec"
    , "avez"
    , "aviez"
    , "avions"
    , "avoir"
    , "avons"
    , "ayant"
    , "ayez"
    , "ayons"
    , "b"
    , "bah"
    , "bas"
    , "basee"
    , "bat"
    , "beau"
    , "beaucoup"
    , "bien"
    , "bigre"
    , "bon"
    , "boum"
    , "bravo"
    , "brrr"
    , "c"
    , "car"
    , "ce"
    , "ceci"
    , "cela"
    , "celle"
    , "celle-ci"
    , "celle-là"
    , "celles"
    , "celles-ci"
    , "celles-là"
    , "celui"
    , "celui-ci"
    , "celui-là"
    , "celà"
    , "cent"
    , "cependant"
    , "certain"
    , "certaine"
    , "certaines"
    , "certains"
    , "certes"
    , "ces"
    , "cet"
    , "cette"
    , "ceux"
    , "ceux-ci"
    , "ceux-là"
    , "chacun"
    , "chacune"
    , "chaque"
    , "cher"
    , "chers"
    , "chez"
    , "chiche"
    , "chut"
    , "chère"
    , "chères"
    , "ci"
    , "cinq"
    , "cinquantaine"
    , "cinquante"
    , "cinquantième"
    , "cinquième"
    , "clac"
    , "clic"
    , "combien"
    , "comme"
    , "comment"
    , "comparable"
    , "comparables"
    , "compris"
    , "concernant"
    , "contre"
    , "couic"
    , "crac"
    , "d"
    , "da"
    , "dans"
    , "de"
    , "debout"
    , "dedans"
    , "dehors"
    , "deja"
    , "delà"
    , "depuis"
    , "dernier"
    , "derniere"
    , "derriere"
    , "derrière"
    , "des"
    , "desormais"
    , "desquelles"
    , "desquels"
    , "dessous"
    , "dessus"
    , "deux"
    , "deuxième"
    , "deuxièmement"
    , "devant"
    , "devers"
    , "devra"
    , "devrait"
    , "different"
    , "differentes"
    , "differents"
    , "différent"
    , "différente"
    , "différentes"
    , "différents"
    , "dire"
    , "directe"
    , "directement"
    , "dit"
    , "dite"
    , "dits"
    , "divers"
    , "diverse"
    , "diverses"
    , "dix"
    , "dix-huit"
    , "dix-neuf"
    , "dix-sept"
    , "dixième"
    , "doit"
    , "doivent"
    , "donc"
    , "dont"
    , "dos"
    , "douze"
    , "douzième"
    , "dring"
    , "droite"
    , "du"
    , "duquel"
    , "durant"
    , "dès"
    , "début"
    , "désormais"
    , "e"
    , "effet"
    , "egale"
    , "egalement"
    , "egales"
    , "eh"
    , "elle"
    , "elle-même"
    , "elles"
    , "elles-mêmes"
    , "en"
    , "encore"
    , "enfin"
    , "entre"
    , "envers"
    , "environ"
    , "es"
    , "essai"
    , "est"
    , "et"
    , "etant"
    , "etc"
    , "etre"
    , "eu"
    , "eue"
    , "eues"
    , "euh"
    , "eurent"
    , "eus"
    , "eusse"
    , "eussent"
    , "eusses"
    , "eussiez"
    , "eussions"
    , "eut"
    , "eux"
    , "eux-mêmes"
    , "exactement"
    , "excepté"
    , "extenso"
    , "exterieur"
    , "eûmes"
    , "eût"
    , "eûtes"
    , "f"
    , "fais"
    , "faisaient"
    , "faisant"
    , "fait"
    , "faites"
    , "façon"
    , "feront"
    , "fi"
    , "flac"
    , "floc"
    , "fois"
    , "font"
    , "force"
    , "furent"
    , "fus"
    , "fusse"
    , "fussent"
    , "fusses"
    , "fussiez"
    , "fussions"
    , "fut"
    , "fûmes"
    , "fût"
    , "fûtes"
    , "g"
    , "gens"
    , "h"
    , "ha"
    , "haut"
    , "hein"
    , "hem"
    , "hep"
    , "hi"
    , "ho"
    , "holà"
    , "hop"
    , "hormis"
    , "hors"
    , "hou"
    , "houp"
    , "hue"
    , "hui"
    , "huit"
    , "huitième"
    , "hum"
    , "hurrah"
    , "hé"
    , "hélas"
    , "i"
    , "ici"
    , "il"
    , "ils"
    , "importe"
    , "j"
    , "je"
    , "jusqu"
    , "jusque"
    , "juste"
    , "k"
    , "l"
    , "la"
    , "laisser"
    , "laquelle"
    , "las"
    , "le"
    , "lequel"
    , "les"
    , "lesquelles"
    , "lesquels"
    , "leur"
    , "leurs"
    , "longtemps"
    , "lors"
    , "lorsque"
    , "lui"
    , "lui-meme"
    , "lui-même"
    , "là"
    , "lès"
    , "m"
    , "ma"
    , "maint"
    , "maintenant"
    , "mais"
    , "malgre"
    , "malgré"
    , "maximale"
    , "me"
    , "meme"
    , "memes"
    , "merci"
    , "mes"
    , "mien"
    , "mienne"
    , "miennes"
    , "miens"
    , "mille"
    , "mince"
    , "mine"
    , "minimale"
    , "moi"
    , "moi-meme"
    , "moi-même"
    , "moindres"
    , "moins"
    , "mon"
    , "mot"
    , "moyennant"
    , "multiple"
    , "multiples"
    , "même"
    , "mêmes"
    , "n"
    , "na"
    , "naturel"
    , "naturelle"
    , "naturelles"
    , "ne"
    , "neanmoins"
    , "necessaire"
    , "necessairement"
    , "neuf"
    , "neuvième"
    , "ni"
    , "nombreuses"
    , "nombreux"
    , "nommés"
    , "non"
    , "nos"
    , "notamment"
    , "notre"
    , "nous"
    , "nous-mêmes"
    , "nouveau"
    , "nouveaux"
    , "nul"
    , "néanmoins"
    , "nôtre"
    , "nôtres"
    , "o"
    , "oh"
    , "ohé"
    , "ollé"
    , "olé"
    , "on"
    , "ont"
    , "onze"
    , "onzième"
    , "ore"
    , "ou"
    , "ouf"
    , "ouias"
    , "oust"
    , "ouste"
    , "outre"
    , "ouvert"
    , "ouverte"
    , "ouverts"
    , "o|"
    , "où"
    , "p"
    , "paf"
    , "pan"
    , "par"
    , "parce"
    , "parfois"
    , "parle"
    , "parlent"
    , "parler"
    , "parmi"
    , "parole"
    , "parseme"
    , "partant"
    , "particulier"
    , "particulière"
    , "particulièrement"
    , "pas"
    , "passé"
    , "pendant"
    , "pense"
    , "permet"
    , "personne"
    , "personnes"
    , "peu"
    , "peut"
    , "peuvent"
    , "peux"
    , "pff"
    , "pfft"
    , "pfut"
    , "pif"
    , "pire"
    , "pièce"
    , "plein"
    , "plouf"
    , "plupart"
    , "plus"
    , "plusieurs"
    , "plutôt"
    , "possessif"
    , "possessifs"
    , "possible"
    , "possibles"
    , "pouah"
    , "pour"
    , "pourquoi"
    , "pourrais"
    , "pourrait"
    , "pouvait"
    , "prealable"
    , "precisement"
    , "premier"
    , "première"
    , "premièrement"
    , "pres"
    , "probable"
    , "probante"
    , "procedant"
    , "proche"
    , "près"
    , "psitt"
    , "pu"
    , "puis"
    , "puisque"
    , "pur"
    , "pure"
    , "q"
    , "qu"
    , "quand"
    , "quant"
    , "quant-à-soi"
    , "quanta"
    , "quarante"
    , "quatorze"
    , "quatre"
    , "quatre-vingt"
    , "quatrième"
    , "quatrièmement"
    , "que"
    , "quel"
    , "quelconque"
    , "quelle"
    , "quelles"
    , "quelqu'un"
    , "quelque"
    , "quelques"
    , "quels"
    , "qui"
    , "quiconque"
    , "quinze"
    , "quoi"
    , "quoique"
    , "r"
    , "rare"
    , "rarement"
    , "rares"
    , "relative"
    , "relativement"
    , "remarquable"
    , "rend"
    , "rendre"
    , "restant"
    , "reste"
    , "restent"
    , "restrictif"
    , "retour"
    , "revoici"
    , "revoilà"
    , "rien"
    , "s"
    , "sa"
    , "sacrebleu"
    , "sait"
    , "sans"
    , "sapristi"
    , "sauf"
    , "se"
    , "sein"
    , "seize"
    , "selon"
    , "semblable"
    , "semblaient"
    , "semble"
    , "semblent"
    , "sent"
    , "sept"
    , "septième"
    , "sera"
    , "serai"
    , "seraient"
    , "serais"
    , "serait"
    , "seras"
    , "serez"
    , "seriez"
    , "serions"
    , "serons"
    , "seront"
    , "ses"
    , "seul"
    , "seule"
    , "seulement"
    , "si"
    , "sien"
    , "sienne"
    , "siennes"
    , "siens"
    , "sinon"
    , "six"
    , "sixième"
    , "soi"
    , "soi-même"
    , "soient"
    , "sois"
    , "soit"
    , "soixante"
    , "sommes"
    , "son"
    , "sont"
    , "sous"
    , "souvent"
    , "soyez"
    , "soyons"
    , "specifique"
    , "specifiques"
    , "speculatif"
    , "stop"
    , "strictement"
    , "subtiles"
    , "suffisant"
    , "suffisante"
    , "suffit"
    , "suis"
    , "suit"
    , "suivant"
    , "suivante"
    , "suivantes"
    , "suivants"
    , "suivre"
    , "sujet"
    , "superpose"
    , "sur"
    , "surtout"
    , "t"
    , "ta"
    , "tac"
    , "tandis"
    , "tant"
    , "tardive"
    , "te"
    , "tel"
    , "telle"
    , "tellement"
    , "telles"
    , "tels"
    , "tenant"
    , "tend"
    , "tenir"
    , "tente"
    , "tes"
    , "tic"
    , "tien"
    , "tienne"
    , "tiennes"
    , "tiens"
    , "toc"
    , "toi"
    , "toi-même"
    , "ton"
    , "touchant"
    , "toujours"
    , "tous"
    , "tout"
    , "toute"
    , "toutefois"
    , "toutes"
    , "treize"
    , "trente"
    , "tres"
    , "trois"
    , "troisième"
    , "troisièmement"
    , "trop"
    , "très"
    , "tsoin"
    , "tsouin"
    , "tu"
    , "té"
    , "u"
    , "un"
    , "une"
    , "unes"
    , "uniformement"
    , "unique"
    , "uniques"
    , "uns"
    , "v"
    , "va"
    , "vais"
    , "valeur"
    , "vas"
    , "vers"
    , "via"
    , "vif"
    , "vifs"
    , "vingt"
    , "vivat"
    , "vive"
    , "vives"
    , "vlan"
    , "voici"
    , "voie"
    , "voient"
    , "voilà"
    , "voire"
    , "vont"
    , "vos"
    , "votre"
    , "vous"
    , "vous-mêmes"
    , "vu"
    , "vé"
    , "vôtre"
    , "vôtres"
    , "w"
    , "x"
    , "y"
    , "z"
    , "zut"
    , "à"
    , "â"
    , "ça"
    , "ès"
    , "étaient"
    , "étais"
    , "était"
    , "étant"
    , "état"
    , "étiez"
    , "étions"
    , "été"
    , "étée"
    , "étées"
    , "étés"
    , "êtes"
    , "être"
    , "ô"
    ]
