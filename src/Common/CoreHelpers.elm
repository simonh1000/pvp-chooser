module Common.CoreHelpers exposing (..)

-- ONLY FOR THINGS THAT ARE TOTALLY UN-REMIX SPECIFIC

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import List as L



---- model/command chaining


ifThenElse : Bool -> a -> a -> a
ifThenElse cond yes no =
    if cond then
        yes

    else
        no


{-| (m, c) |> addCmd c2
-}
addCmd : Cmd msg -> ( m, Cmd msg ) -> ( m, Cmd msg )
addCmd c ( m1, c1 ) =
    ( m1, Cmd.batch [ c, c1 ] )


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


{-| Removes a list of keys from a Dict
-}
rejectByList : List comparable -> Dict comparable a -> Dict comparable a
rejectByList lst dict =
    List.foldl Dict.remove dict lst



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
