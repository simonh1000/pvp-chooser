module Helpers exposing (..)

import Array
import AssocList as Dict exposing (Dict)
import Common.CoreHelpers exposing (foldResult)
import List as L
import Model exposing (..)
import Pokemon exposing (PType, effectiveness)
import Set



-- calculate team scores


mkTeams : List a -> List ( a, a, a )
mkTeams lst =
    let
        inner tl =
            case tl of
                hd :: tl_ ->
                    L.map (\x -> ( hd, x )) tl_ ++ inner tl_

                [] ->
                    []
    in
    case lst of
        [] ->
            []

        hd :: tl ->
            L.map (\( x, y ) -> ( hd, x, y )) (inner tl) ++ mkTeams tl


evaluateTeams : League -> List ( ( String, String, String ), Float )
evaluateTeams league =
    let
        mapper (( p1, p2, p3 ) as team) =
            ( ( p1.name, p2.name, p3.name )
            , (summariseTeam league.opponents <| evaluateTeam team) / calcWeightedTotal league.opponents
            )
    in
    league.myPokemon
        |> Array.toList
        |> mkTeams
        |> L.map mapper
        |> L.sortBy (Tuple.second >> (*) -1)


calcWeightedTotal opponents =
    opponents |> L.map Tuple.second |> L.sum |> toFloat


{-| Always divide by 3 even though for weak pokemon we only return two (low) numbers
-}
summariseTeam : List ( String, Int ) -> Dict String Float -> Float
summariseTeam opponents scores =
    let
        go ( name, freq ) acc =
            scores
                |> Dict.get name
                |> Maybe.withDefault 0
                |> (*) (toFloat freq)
                |> (+) acc
    in
    L.foldl go 0 opponents


evaluateTeam : ( Pokemon, Pokemon, Pokemon ) -> Dict String Float
evaluateTeam ( p1, p2, p3 ) =
    let
        evalTeam scores =
            let
                weaks =
                    L.filter (\s -> s < 0.9) scores
            in
            if L.length weaks > 2 then
                -3

            else if L.length weaks > 1 then
                -1

            else
                L.sum scores / 3
    in
    Dict.foldl
        (\name s1 acc ->
            case Maybe.map2 (\s2 s3 -> evalTeam [ s1, s2, s3 ]) (Dict.get name p2.scores) (Dict.get name p3.scores) of
                Just score ->
                    Dict.insert name score acc

                Nothing ->
                    acc
        )
        Dict.empty
        p1.scores



-- Calculating individual scores


addScoresToLeague : Model -> League -> League
addScoresToLeague model league =
    let
        addScores_ : Pokemon -> Dict String Float
        addScores_ p =
            let
                foldFn : ( String, b ) -> Dict String Float -> Result String (Dict String Float)
                foldFn ( opName, _ ) acc =
                    case evaluateBattle model.pokedex model.attacks p opName of
                        Ok score ->
                            Ok <| Dict.insert opName score acc

                        Err err ->
                            Err err

                mbRes =
                    foldResult foldFn (Ok Dict.empty) league.opponents
            in
            case mbRes of
                Ok res ->
                    res

                Err err ->
                    Debug.todo err
    in
    { league | myPokemon = Array.map (\p -> { p | scores = addScores_ p }) league.myPokemon }


evaluateBattle : Pokedex -> Dict String MoveType -> Pokemon -> String -> Result String Float
evaluateBattle pokedex attacks pokemon opName =
    let
        handler myPokedexEntry opponent =
            let
                attackScore =
                    evaluateAgainstOpponent attacks pokemon opponent.types

                defenceScore =
                    evaluateAverageEffect attacks (opponent.fast ++ opponent.charged) myPokedexEntry.types
            in
            attackScore / defenceScore
    in
    Maybe.map2 handler (Dict.get pokemon.name pokedex) (Dict.get opName pokedex)
        |> Result.fromMaybe ("could not look up one of : " ++ opName ++ ", or " ++ pokemon.name)


evaluateAgainstOpponent : Dict String MoveType -> Pokemon -> List PType -> Float
evaluateAgainstOpponent attacks pokemon opponentTypes =
    let
        lookup attack =
            attack
                |> lookupMatrix attacks
                |> Maybe.map (calculateEffectiveness opponentTypes)
                |> Maybe.withDefault -100
                |> Tuple.pair attack

        bestCharged =
            pokemon.charged
                |> Set.toList
                |> L.map lookup
                |> L.sortBy (\( name, score ) -> score * -1)
                |> L.head
                |> Maybe.map Tuple.second
                |> Maybe.withDefault -100

        fastAttack =
            lookup pokemon.fast |> Tuple.second
    in
    (bestCharged + fastAttack) / 2


evaluateAverageEffect : Dict String MoveType -> List String -> List PType -> Float
evaluateAverageEffect attacks attackNames opponentTypes =
    let
        lookup attack =
            attack
                |> lookupMatrix attacks
                |> Maybe.map (calculateEffectiveness opponentTypes)
                |> Maybe.withDefault -100
    in
    attackNames
        |> L.map lookup
        |> L.sum
        |> (\total -> total / (toFloat <| L.length attackNames))


lookupMatrix : Dict String MoveType -> String -> Maybe (Dict PType Float)
lookupMatrix attacks attack =
    Dict.get attack attacks
        |> Maybe.andThen (\moveType -> Dict.get moveType.type_ effectiveness)


calculateEffectiveness : List PType -> Dict PType Float -> Float
calculateEffectiveness defenderTypes matrix =
    let
        go : PType -> Float -> Float
        go dType acc =
            matrix
                |> Dict.get dType
                |> Maybe.withDefault 1
                |> (*) acc
    in
    L.foldl go 1 defenderTypes
