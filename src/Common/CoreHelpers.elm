module Common.CoreHelpers exposing (..)

-- ONLY FOR THINGS THAT ARE TOTALLY UN-REMIX SPECIFIC

import Json.Decode as Decode exposing (Decoder)
import List as L



-- Regex
--rgxContains : String -> String -> Bool
--rgxContains rgxString str =
--    Regex.fromString rgxString
--        |> Maybe.map (\rgx -> Regex.contains rgx str)
--        |> Maybe.withDefault False
--
--
--rgxFind : String -> String -> List Regex.Match
--rgxFind rgxString tgt =
--    Regex.fromString rgxString
--        |> Maybe.map (\rgx -> Regex.find rgx tgt)
--        |> Maybe.withDefault []
--
--
--rgxReplace : String -> (Regex.Match -> String) -> String -> String
--rgxReplace rgxString fn orig =
--    Regex.fromString rgxString
--        |> Maybe.map (\rgx -> Regex.replace rgx fn orig)
--        |> Maybe.withDefault orig
--
--
--rgxSplitAtMost1 : String -> String -> List String
--rgxSplitAtMost1 rgxString orig =
--    Regex.fromString rgxString
--        |> Maybe.map (\rgx -> Regex.splitAtMost 1 rgx orig)
--        |> Maybe.withDefault []
--
--
--
---- model/command chaining


ifThenElse : Bool -> a -> a -> a
ifThenElse cond yes no =
    if cond then
        yes

    else
        no



--{-| map the 3rd element of a tuple
---}
--mapThird : (c -> d) -> ( a, b, c ) -> ( a, b, d )
--mapThird fn ( a, b, c ) =
--    ( a, b, fn c )
--
--
--{-| (m, c) |> updateAndThen (produceModAndCmd m)
---}
--updateAndThen : (m -> ( m, Cmd msg )) -> ( m, Cmd msg ) -> ( m, Cmd msg )
--updateAndThen fn ( m1, c1 ) =
--    let
--        ( m2, c2 ) =
--            fn m1
--    in
--    ( m2, Cmd.batch [ c1, c2 ] )
--
--
--updateWithParentMsg : (msg -> m -> ( m, Cmd msg )) -> ( m, Maybe msg ) -> ( m, Cmd msg )
--updateWithParentMsg update ( m, mbMsg ) =
--    case mbMsg of
--        Just msg ->
--            update msg m
--
--        Nothing ->
--            ( m, Cmd.none )
--
--
--updateWithParentMsg2 : (msg -> m -> ( m, Cmd msg )) -> ( m, Cmd msg ) -> Maybe msg -> ( m, Cmd msg )
--updateWithParentMsg2 update res mbMsg =
--    case mbMsg of
--        Just msg_ ->
--            res |> updateAndThen (update msg_)
--
--        Nothing ->
--            res
--
--
--updateWithParentMsg3 : (msg -> m -> ( m, Cmd msg )) -> ( m, Cmd msg, Maybe msg ) -> ( m, Cmd msg )
--updateWithParentMsg3 update ( m, c, mbMsg ) =
--    case mbMsg of
--        Just msg_ ->
--            ( m, c ) |> updateAndThen (update msg_)
--
--        Nothing ->
--            ( m, c )
--
--
--{-| (m, c) |> andAddCmd (produceCmd m)
---}
--andAddCmd : (m -> Cmd msg) -> ( m, Cmd msg ) -> ( m, Cmd msg )
--andAddCmd fn ( m, c ) =
--    ( m, Cmd.batch [ c, fn m ] )
--
--


{-| (m, c) |> addCmd c2
-}
addCmd : Cmd msg -> ( m, Cmd msg ) -> ( m, Cmd msg )
addCmd c ( m1, c1 ) =
    ( m1, Cmd.batch [ c, c1 ] )



--
--
--addBoth : m -> ( m, Cmd msg, Maybe a )
--addBoth =
--    addNone >> addNothing
--
--
--{-| m |> addNone
---}
--addNone : m -> ( m, Cmd msg )
--addNone m =
--    ( m, Cmd.none )
--
--
--addNothing : ( m, cmd ) -> ( m, cmd, Maybe msg )
--addNothing ( m, c ) =
--    ( m, c, Nothing )
--
--
--{-| Turns a msg into a Cmd msg
---}
--mkCmd : msg -> Cmd msg
--mkCmd m =
--    Task.perform (always m) (Task.succeed ())
--
--
--
---- STRINGS
--
--
--stringFromBool : Bool -> String
--stringFromBool t =
--    if t then
--        "True"
--
--    else
--        "False"
--
--
--escapeString : String -> String
--escapeString =
--    Encode.string >> Encode.encode 0
--
--
--{-| Format plural for nouns which have an irregular plural
--
--  - 0, child, children -> 0 children
--  - 1, child, children -> 1 child
--  - 10, child, children -> 10 children
--
---}
--formatPluralIrregular : Int -> String -> String -> String
--formatPluralIrregular nb singular plural =
--    if nb == 0 then
--        "0 " ++ plural
--
--    else if nb == 1 then
--        "1 " ++ singular
--
--    else
--        String.fromInt nb ++ " " ++ plural
--
--
--{-| Format plural for nouns which have a regular plural (meaning the plural is the singular+ 's'
--
--  - 0, day -> 0 days
--  - 1, day -> 1 day
--  - 10, day -> 10 day
--
---}
--formatPluralRegular : Int -> String -> String
--formatPluralRegular nb singular =
--    formatPluralIrregular nb singular (singular ++ "s")
--
--
--{-| Format plural for nouns which have an irregular plural
--
--  - 0, child, children -> no children
--  - 1, child, children -> one child
--  - 10, child, children -> 10 children
--
---}
--formatPluralIrregularAlt : Int -> String -> String -> String
--formatPluralIrregularAlt nb singular plural =
--    if nb == 0 then
--        "no " ++ plural
--
--    else if nb == 1 then
--        "one " ++ singular
--
--    else
--        String.fromInt nb ++ " " ++ plural
--
--
--{-| Format plural for nouns which have a regular plural (meaning the plural is the singular+ 's'
--
--  - 0, day -> no days
--  - 1, day -> one day
--  - 10, day -> 10 day
--
---}
--formatPluralRegularAlt : Int -> String -> String
--formatPluralRegularAlt nb singular =
--    formatPluralIrregularAlt nb singular (singular ++ "s")
--
--
--
---- Json Decoder
--
--
--decodeOnError : (String -> Decoder a) -> Decoder a -> Decoder a
--decodeOnError fn dec =
--    Decode.value
--        |> Decode.andThen
--            (\val ->
--                case Decode.decodeValue dec val of
--                    Ok res ->
--                        Decode.succeed res
--
--                    Err err ->
--                        fn <| Decode.errorToString err
--            )
--
--
--resToDecoder : Result String a -> Decoder a
--resToDecoder res =
--    res |> Result.map Decode.succeed |> RE.extract Decode.fail
--
--
--decodeSimpleCustomType : String -> a -> Decoder a
--decodeSimpleCustomType tgt tp =
--    exactMatchString Decode.string tgt (Decode.succeed tp)
--
--


{-| decodes custom types that have no parameters
-}
decodeSimpleCustomTypes : List ( String, a ) -> Decoder a
decodeSimpleCustomTypes lst =
    let
        dec ( tgt, res ) =
            exactMatchString Decode.string tgt (Decode.succeed res)
    in
    lst |> L.map dec |> Decode.oneOf


{-| Useful for decoding AST in that it allows you to check for the existence
of a string matching some constructor before proceeding further
-}
exactMatch : String -> String -> Decoder a -> Decoder a
exactMatch fieldname tgt dec =
    exactMatchString (Decode.field fieldname Decode.string) tgt dec


exactMatchString : Decoder String -> String -> Decoder a -> Decoder a
exactMatchString matchDecoder match dec =
    matchDecoder
        |> Decode.andThen
            (\str ->
                if str == match then
                    dec

                else
                    Decode.fail <| "[exactMatch2] tgt: " ++ match ++ " /= " ++ str
            )


exactMatchGeneral : Decoder a -> a -> Decoder b -> Decoder b
exactMatchGeneral matchDecoder match dec =
    matchDecoder
        |> Decode.andThen
            (\val ->
                if val == match then
                    dec

                else
                    Decode.fail <| "[exactMatch3] no match found"
            )



--
---- MAYBE
--
--
--foldMaybe : (a -> b -> Maybe b) -> Maybe b -> List a -> Maybe b
--foldMaybe f bMaybe lst =
--    L.foldl (\a acc -> acc |> Maybe.andThen (f a)) bMaybe lst
--
--
--foldRMaybe : (a -> b -> Maybe b) -> Maybe b -> List a -> Maybe b
--foldRMaybe f bMaybe lst =
--    L.foldr (\a acc -> acc |> Maybe.andThen (f a)) bMaybe lst
--
--
-- RESULT


foldResult : (a -> b -> Result e b) -> Result e b -> List a -> Result e b
foldResult f bResult lst =
    L.foldl (\a acc -> acc |> Result.andThen (f a)) bResult lst


foldRResult : (a -> b -> Result e b) -> Result e b -> List a -> Result e b
foldRResult f bResult lst =
    L.foldr (\a acc -> acc |> Result.andThen (f a)) bResult lst



--
--mapResult : (a -> Result b c) -> List a -> Result b (List c)
--mapResult fn lst =
--    let
--        go : a -> List c -> Result b (List c)
--        go item acc =
--            Result.map (\item_ -> item_ :: acc) (fn item)
--    in
--    foldRResult go (Ok []) lst
--
--
--taskFromResult : Result x a -> Task x a
--taskFromResult res =
--    case res of
--        Ok a ->
--            Task.succeed a
--
--        Err x ->
--            Task.fail x
--
--
--
---- LISTS
--
--
--slice : Int -> Int -> List a -> List a
--slice offset howMany =
--    L.drop offset >> L.take howMany
--
--
--removeFromList : List a -> List a -> List a
--removeFromList tgts lst =
--    L.filter (not << flip L.member tgts) lst
--
--
--attemptUpdateNth : (a -> Maybe a) -> Int -> List a -> Maybe (List a)
--attemptUpdateNth fn idx lst =
--    case L.drop idx lst of
--        hd :: tl ->
--            case fn hd of
--                Just hd_ ->
--                    Just <| L.take idx lst ++ hd_ :: tl
--
--                Nothing ->
--                    Nothing
--
--        [] ->
--            Nothing
--
--
--{-| Inserts at index, pushing subsequent elements back
--When index > Length of lst then append to end
---}
--insertNth : Int -> a -> List a -> List a
--insertNth idx a lst =
--    case L.drop idx lst of
--        [] ->
--            lst ++ [ a ]
--
--        tl ->
--            L.take idx lst ++ (a :: tl)
--
--
--{-| return list of duplicate items in a list
--`["a", "a", "b", "c", "d", "d", "d"] -> ["a", "d"]`
---}
--detectDuplicates : List comparable -> List comparable
--detectDuplicates lst =
--    lst
--        |> groupListBy identity
--        |> L.filter (\l -> L.length l > 1)
--        |> L.filterMap L.head
--
--
--groupListBy : (a -> comparable) -> List a -> List (List a)
--groupListBy fn =
--    let
--        go item acc =
--            let
--                key =
--                    fn item
--
--                newItemAcc =
--                    case Dict.get key acc of
--                        Just accItem ->
--                            item :: accItem
--
--                        Nothing ->
--                            [ item ]
--            in
--            Dict.insert key newItemAcc acc
--    in
--    L.foldr go Dict.empty >> Dict.values
--
--
--maybeCons : Maybe a -> List a -> List a
--maybeCons mb xs =
--    case mb of
--        Just x ->
--            x :: xs
--
--        Nothing ->
--            xs
--
--
--
---- --------------------
---- Dict
---- --------------------
--
--
--insertIfMissing : comparable -> a -> Dict comparable a -> Dict comparable a
--insertIfMissing key val dict =
--    case Dict.get key dict of
--        Just _ ->
--            dict
--
--        Nothing ->
--            Dict.insert key val dict
--
--
--{-| Gets smaller dict from a larger one
---}
--filterByList : List comparable -> Dict comparable a -> Dict comparable a
--filterByList lst dict =
--    let
--        folder fieldName acc =
--            case Dict.get fieldName dict of
--                Just field ->
--                    Dict.insert fieldName field acc
--
--                Nothing ->
--                    acc
--    in
--    L.foldl folder Dict.empty lst
--
--
--
--{-| Removes a list of keys from a Dict
---}
--rejectByList : List comparable -> Dict comparable a -> Dict comparable a
--rejectByList lst dict =
--    List.foldl Dict.remove dict lst
--renameKey : comparable -> comparable -> Dict comparable b -> Dict comparable b
--renameKey oldKey newKey dict =
--    case Dict.get oldKey dict of
--        Just val ->
--            dict |> Dict.insert newKey val |> Dict.remove oldKey
--
--        Nothing ->
--            dict
--
--
--
---- --------------------
---- Array
---- --------------------
--
--
--arrayIndexedFoldl : (Int -> a -> b -> b) -> b -> Array a -> b
--arrayIndexedFoldl fn b array =
--    let
--        go a ( ct, acc ) =
--            ( ct + 1, fn ct a acc )
--    in
--    Array.foldl go ( 0, b ) array |> Tuple.second
--
--
--arrayInsertAt : Int -> a -> Array a -> Array a
--arrayInsertAt idx n arr =
--    if idx > Array.length arr then
--        Array.push n arr
--
--    else
--        Array.append (Array.push n (Array.slice 0 idx arr)) (Array.slice idx (Array.length arr) arr)
--
