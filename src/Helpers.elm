module Helpers exposing (..)

import Array exposing (Array)
import AssocList as Dict exposing (Dict)
import Common.CoreHelpers exposing (foldResult)
import List as L
import Model exposing (..)
import Pokemon exposing (PType, effectiveness)
import Set


{-| Creates permutations of 3 different members of a list
-}
mkTeams3 : List a -> List ( a, a, a )
mkTeams3 lst =
    case lst of
        [] ->
            []

        hd :: tl ->
            L.map (\( x, y ) -> ( hd, x, y )) (mkTeams2 tl) ++ mkTeams3 tl


mkTeams2 : List a -> List ( a, a )
mkTeams2 tl =
    case tl of
        hd :: tl_ ->
            L.map (\x -> ( hd, x )) tl_ ++ mkTeams2 tl_

        [] ->
            []



-- calculate team scores


evaluateTeams : League -> List ( ( String, String, String ), Float )
evaluateTeams league =
    let
        sumFreqs =
            calcWeightedTotal league.opponents

        mapper (( p1, p2, p3 ) as team) =
            ( ( p1.speciesId, p2.speciesId, p3.speciesId )
            , (summariseTeam league.opponents <| evaluateTeam team) / sumFreqs
            )

        teams =
            -- built taking into account which team members are pinned
            case mkTeamList league.team |> L.filterMap getPinnedMember |> L.filterMap (lookupName league.myPokemon >> Result.toMaybe) of
                [ p1 ] ->
                    league.myPokemon
                        |> Array.toList
                        |> L.filter (\p -> p.speciesId /= p1.speciesId)
                        |> mkTeams2
                        |> L.map (\( p2, p3 ) -> ( p1, p2, p3 ))

                [ p1, p2 ] ->
                    league.myPokemon
                        |> Array.toList
                        |> L.filter (\p -> p.speciesId /= p1.speciesId && p.speciesId /= p2.speciesId)
                        |> L.map (\p3 -> ( p1, p2, p3 ))

                [ p1, p2, p3 ] ->
                    [ ( p1, p2, p3 ) ]

                _ ->
                    league.myPokemon
                        |> Array.toList
                        |> mkTeams3
    in
    teams
        |> L.map mapper
        |> L.sortBy (Tuple.second >> (*) -1)


lookupName : Array Pokemon -> String -> Result String Pokemon
lookupName myPokemon name =
    myPokemon
        |> Array.filter (\item -> item.speciesId == name)
        |> Array.get 0
        |> Result.fromMaybe ("Could not lookup: " ++ name)


calcWeightedTotal : Dict String Opponent -> Float
calcWeightedTotal opponents =
    opponents |> Dict.values |> L.map .frequency |> L.sum |> toFloat


{-| Calculates the weighted sum of scores against opponents
-}
summariseTeam : Dict String Opponent -> Dict String Float -> Float
summariseTeam opponents scores =
    let
        go name { frequency } acc =
            scores
                |> Dict.get name
                |> Maybe.withDefault 0
                |> (*) (toFloat frequency)
                |> (+) acc
    in
    Dict.foldl go 0 opponents


{-| Looks at how we a team will do against each opponent. Teams that are all weak to a particular opponent
are particularly penalised.
-}
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
                foldFn : ( String, Opponent ) -> Dict String Float -> Result String (Dict String Float)
                foldFn ( speciesId, _ ) acc =
                    case evaluateBattle model.pokedex model.attacks p speciesId of
                        Ok score ->
                            Ok <| Dict.insert speciesId score acc

                        Err err ->
                            Err err

                mbRes =
                    league.opponents
                        |> Dict.toList
                        |> foldResult foldFn (Ok Dict.empty)
            in
            case mbRes of
                Ok res ->
                    res

                Err _ ->
                    Dict.empty
    in
    { league | myPokemon = Array.map (\p -> { p | scores = addScores_ p }) league.myPokemon }


evaluateBattle : Pokedex -> Dict String MoveType -> Pokemon -> String -> Result String Float
evaluateBattle pokedex attacks pokemon opSpeciesId =
    let
        handler myDexEntry opDexEntry =
            let
                attackScore =
                    evaluateAgainstOpponent attacks pokemon opDexEntry.types

                defenceScore =
                    evaluateOpponentAttacks attacks opDexEntry myDexEntry.types
            in
            attackScore / defenceScore
    in
    Maybe.map2 handler (Dict.get pokemon.speciesId pokedex) (Dict.get opSpeciesId pokedex)
        |> Result.fromMaybe ("could not look up one of : " ++ opSpeciesId ++ ", or " ++ pokemon.speciesId)


{-| Calculates effect of my pokemon's chosen attacks on an opponent
-}
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
                |> L.sortBy (\( _, score ) -> score * -1)
                |> L.head
                |> Maybe.map Tuple.second
                |> Maybe.withDefault -100

        fastAttack =
            lookup pokemon.fast |> Tuple.second
    in
    (bestCharged + fastAttack) / 2


{-| Calculates effect of opponent's pokemons' (weighted) average attacks on my pokemon
-}
evaluateOpponentAttacks : Dict String MoveType -> PokedexEntry -> List PType -> Float
evaluateOpponentAttacks attacks opDexEntry myTypes =
    let
        pvPokeMultiplier =
            3

        isPvPoke attk score =
            if Just attk == opDexEntry.recFast || L.member (Just attk) opDexEntry.recsCharged then
                score * pvPokeMultiplier

            else
                score

        opAttackNames =
            opDexEntry.fast ++ opDexEntry.charged

        lookup attack =
            attack
                |> lookupMatrix attacks
                |> Maybe.map (calculateEffectiveness myTypes >> isPvPoke attack)
                -- multiply by likelihood of having been chosen
                |> Maybe.withDefault -100

        denominator =
            L.length opAttackNames + ((pvPokeMultiplier - 1) * L.length (L.filterMap identity <| opDexEntry.recFast :: opDexEntry.recsCharged))
    in
    opAttackNames
        |> L.map lookup
        |> L.sum
        |> (\total -> total / toFloat denominator)


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
