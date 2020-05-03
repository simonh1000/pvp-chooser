module Helpers exposing (..)

import Array
import AssocList as Dict exposing (Dict)
import List as L
import Model exposing (..)
import Pokemon exposing (PType, effectiveness)
import Set


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
    league.myPokemon
        |> Array.toList
        |> mkTeams
        |> L.map (\(( p1, p2, p3 ) as team) -> ( ( p1.name, p2.name, p3.name ), summariseTeam <| evaluateTeam team ))
        |> L.sortBy (Tuple.second >> (*) -1)


summariseTeam : Dict String Float -> Float
summariseTeam opponentScores =
    L.sum (Dict.values opponentScores) / (toFloat <| Dict.size opponentScores)


evaluateTeam : ( Pokemon, Pokemon, Pokemon ) -> Dict String Float
evaluateTeam ( p1, p2, p3 ) =
    let
        evalTeam scores =
            let
                weaks =
                    L.filter (\s -> s < 0.9) scores
            in
            if L.length weaks > 1 then
                L.sum weaks

            else
                L.sum scores
    in
    Dict.foldl
        (\name s1 acc ->
            case Maybe.map2 (\s2 s3 -> evalTeam [ s1, s2, s3 ]) (Dict.get name p2.scores) (Dict.get name p3.scores) of
                Just score ->
                    Dict.insert name (score / 3) acc

                Nothing ->
                    acc
        )
        Dict.empty
        p1.scores


addScoresToLeague : Model -> League -> League
addScoresToLeague model league =
    let
        opponents =
            league.myPokemon
                |> Array.toList
                |> L.map .name
                |> (++) league.opponents
                |> mkOpponentTypes model.pokedex
    in
    { league | myPokemon = Array.map (addScores model opponents) league.myPokemon }


addScores : Model -> Dict String (List PType) -> Pokemon -> Pokemon
addScores model opponents pokemon =
    { pokemon
        | scores =
            Dict.map (\_ defenderTypes -> evaluateAgainstOpponent model pokemon defenderTypes) opponents
    }


mkOpponentTypes : Pokedex -> List String -> Dict String (List PType)
mkOpponentTypes pokedex opponents =
    let
        go : String -> Dict String (List PType) -> Dict String (List PType)
        go opponent acc =
            case Dict.get opponent pokedex of
                Just entry ->
                    Dict.insert opponent entry.types acc

                Nothing ->
                    acc
    in
    L.foldl go Dict.empty opponents


evaluateAgainstOpponent : Model -> Pokemon -> List PType -> Float
evaluateAgainstOpponent model pokemon defenderTypes =
    let
        bestCharged =
            pokemon.charged
                |> Set.toList
                |> L.filterMap (lookupMatrix model.attacks effectiveness)
                |> L.map (calculateEffectiveness defenderTypes)
                |> L.head
                |> Maybe.withDefault -100

        fastAttack =
            pokemon.fast
                |> lookupMatrix model.attacks effectiveness
                |> Maybe.map (calculateEffectiveness defenderTypes)
                |> Maybe.withDefault -100
    in
    (bestCharged + fastAttack) / 2


lookupMatrix : Dict String MoveType -> Effectiveness -> String -> Maybe (Dict PType Float)
lookupMatrix attacks effectiveness attack =
    Dict.get attack attacks
        |> Maybe.andThen
            (\moveType ->
                Dict.get moveType.type_ effectiveness
            )


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
