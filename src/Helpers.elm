module Helpers exposing (..)

import Array
import AssocList as Dict exposing (Dict)
import List as L
import Model exposing (..)
import Pokemon exposing (PType)
import Set


mkTeams : List a -> List ( a, a, a )
mkTeams lst =
    let
        mkThree ( x, y ) =
            L.map (\mem -> ( x, y, mem ))
    in
    case lst of
        m1 :: m2 :: tl ->
            mkThree ( m1, m2 ) tl ++ mkTeams (m2 :: tl)

        _ ->
            []



--


evaluateTeams : Model -> League -> List ( ( a, a, a ), Float )
evaluateTeams model league =
    let
        candidates : List ( Pokemon, Pokemon, Pokemon )
        candidates =
            league.myPokemon
                |> Array.toList
                |> mkTeams

        --scoreTeam : List (List Pokemon, List (String, PType))
        --scoreTeam =
        --    teams
        --        |> L.filterMap
        --            (\t ->
        --                case t of
        --                    [ _, _, _ ] ->
        --                        summariseTeamInner model.attacks t
        --                            |> Tuple.pair t
        --                            |> Just
        --
        --                    _ ->
        --                        Nothing
        --            )
        --
        --team : List ( String, PType )
        --team =
        --    summariseTeam model league
    in
    []


summariseTeam : Dict String Float -> Float
summariseTeam dict =
    L.sum (Dict.values dict) / (toFloat <| Dict.size dict)


evaluateTeam : ( Pokemon, Pokemon, Pokemon ) -> Dict String Float
evaluateTeam ( p1, p2, p3 ) =
    let
        evalTeam : List Float -> Float
        evalTeam scores =
            let
                weaks =
                    L.length <| L.filter (\s -> s < 0.9) scores

                strongs =
                    L.length <| L.filter (\s -> s < 0.9) scores
            in
            if weaks > 1 then
                -2

            else
                toFloat strongs
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


addScoresToLeague : Model -> League -> League
addScoresToLeague model league =
    let
        opponents =
            mkOpponentTypes model.pokedex league.opponents
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
                |> L.filterMap (lookupMatrix model.attacks model.effectiveness)
                |> L.map (calculateEffectiveness defenderTypes)
                |> L.head
                |> Maybe.withDefault -100

        fastAttack =
            pokemon.fast
                |> lookupMatrix model.attacks model.effectiveness
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
