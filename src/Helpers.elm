module Helpers exposing (..)

import Array
import AssocList as Dict exposing (Dict)
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
                |> Debug.log ""
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
                -1

            else if L.length weaks > 1 then
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



-- Calculating individual scores


addScoresToLeague : Model -> League -> League
addScoresToLeague model league =
    let
        opponents =
            league.opponents
                |> mkOpponentTypes model.pokedex
    in
    { league | myPokemon = Array.map (addScores model opponents) league.myPokemon }


mkOpponentTypes : Pokedex -> List ( String, Int ) -> Dict String (List PType)
mkOpponentTypes pokedex opponents =
    let
        go : ( String, Int ) -> Dict String (List PType) -> Dict String (List PType)
        go ( opName, _ ) acc =
            case Dict.get opName pokedex of
                Just entry ->
                    Dict.insert opName entry.types acc

                Nothing ->
                    acc
    in
    L.foldl go Dict.empty opponents


addScores : Model -> Dict String (List PType) -> Pokemon -> Pokemon
addScores model opponents pokemon =
    { pokemon
        | scores =
            Dict.map (\_ defenderTypes -> evaluateAgainstOpponent model.attacks pokemon defenderTypes) opponents
    }


evaluateBattle : Pokedex -> Dict String MoveType -> Pokemon -> String -> Result String Float
evaluateBattle pokedex attacks pokemon opname =
    let
        handler myPokedexEntry opponent =
            let
                attackScore =
                    evaluateAgainstOpponent attacks pokemon opponent.types
                        |> Debug.log "attack"

                defenceScore =
                    evaluateAverageEffect attacks (opponent.fast ++ opponent.charged) myPokedexEntry.types
                        |> Debug.log "defence"
            in
            attackScore / defenceScore
    in
    Maybe.map2 handler (Dict.get pokemon.name pokedex) (Dict.get opname pokedex)
        |> Result.fromMaybe ("could not look up one of : " ++ opname ++ ", or " ++ pokemon.name)


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
