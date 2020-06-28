module Helpers exposing (..)

import AssocList as AL
import Common.CoreHelpers exposing (foldResult)
import Dict exposing (Dict)
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


evaluateTeams : League -> List ( Team, Float )
evaluateTeams league =
    let
        sumFreqs =
            calcWeightedTotal league.opponents

        mapper ps =
            (summariseTeam league.opponents <| evaluateTeam ps) / sumFreqs

        pinnedTeam =
            getPinnedTeam league.team

        newTeam lst =
            L.foldl (addToTeam << Chosen) pinnedTeam lst

        pinnedMembers : List ( String, Pokemon )
        pinnedMembers =
            mkTeamList league.team
                |> L.filterMap getPinnedMember
                |> L.filterMap (\n -> lookupName league.myPokemon n |> Result.toMaybe |> Maybe.map (Tuple.pair n))

        teams : List ( Team, ( Pokemon, Pokemon, Pokemon ) )
        teams =
            -- built taking into account which team members are pinned
            case pinnedMembers of
                [ ( s1, p1 ) ] ->
                    league.myPokemon
                        |> Dict.toList
                        |> L.filter (\( speciesId, _ ) -> speciesId /= s1)
                        |> mkTeams2
                        |> L.map (\( ( s2, p2 ), ( s3, p3 ) ) -> ( newTeam [ s2, s3 ], ( p1, p2, p3 ) ))

                [ ( s1, p1 ), ( s2, p2 ) ] ->
                    league.myPokemon
                        |> Dict.toList
                        |> L.filter (\( speciesId, _ ) -> speciesId /= s1 && speciesId /= s2)
                        |> L.map (\( s3, p3 ) -> ( newTeam [ s3 ], ( p1, p2, p3 ) ))

                [ ( _, p1 ), ( _, p2 ), ( _, p3 ) ] ->
                    [ ( pinnedTeam, ( p1, p2, p3 ) ) ]

                _ ->
                    league.myPokemon
                        |> Dict.toList
                        |> mkTeams3
                        |> L.map (\( ( s1, p1 ), ( s2, p2 ), ( s3, p3 ) ) -> ( newTeam [ s1, s2, s3 ], ( p1, p2, p3 ) ))
    in
    teams
        |> L.map (Tuple.mapSecond mapper)
        |> L.sortBy (Tuple.second >> (*) -1)


lookupName : Dict String Pokemon -> String -> Result String Pokemon
lookupName myPokemon speciesId =
    myPokemon
        |> Dict.get speciesId
        |> Result.fromMaybe ("Could not lookup: " ++ speciesId)


lookup2 : Dict String MyPokemonData -> String -> Result String Pokemon
lookup2 myPokemon speciesId =
    myPokemon
        |> Dict.get speciesId
        |> Result.fromMaybe ("Could not lookup: " ++ speciesId)
        |> Result.map .pokemon


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
        addScores_ : String -> Pokemon -> Dict String Float
        addScores_ speciesId p =
            let
                foldFn : ( String, Opponent ) -> Dict String Float -> Result String (Dict String Float)
                foldFn ( opSpeciesId, _ ) acc =
                    case evaluateBattle model.pokedex model.moves speciesId p opSpeciesId of
                        Ok score ->
                            Ok <| Dict.insert opSpeciesId score acc

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
    { league | myPokemon = Dict.map (\speciesId p -> { p | scores = addScores_ speciesId p }) league.myPokemon }


evaluateBattle : Pokedex -> Dict String MoveType -> String -> Pokemon -> String -> Result String Float
evaluateBattle pokedex attacks speciesId pokemon opSpeciesId =
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
    Maybe.map2 handler (Dict.get speciesId pokedex) (Dict.get opSpeciesId pokedex)
        |> Result.fromMaybe ("could not look up one of : " ++ opSpeciesId ++ ", or " ++ speciesId)


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
evaluateOpponentAttacks attacks entry myTypes =
    let
        pvPokeMultiplier =
            3

        isPvPoke attack score =
            if L.member attack entry.recMoves then
                score * pvPokeMultiplier

            else
                score

        opAttackNames =
            entry.fast ++ entry.charged

        lookup attack =
            attack
                |> lookupMatrix attacks
                |> Maybe.map (calculateEffectiveness myTypes >> isPvPoke attack)
                -- multiply by likelihood of having been chosen
                |> Maybe.withDefault -100

        denominator =
            L.length opAttackNames + ((pvPokeMultiplier - 1) * L.length entry.recMoves)
    in
    opAttackNames
        |> L.map lookup
        |> L.sum
        |> (\total -> total / toFloat denominator)


lookupMatrix : Dict String MoveType -> String -> Maybe (AL.Dict PType Float)
lookupMatrix attacks attack =
    Dict.get attack attacks
        |> Maybe.andThen (\moveType -> AL.get moveType.type_ effectiveness)


calculateEffectiveness : List PType -> AL.Dict PType Float -> Float
calculateEffectiveness defenderTypes matrix =
    let
        go : PType -> Float -> Float
        go dType acc =
            matrix
                |> AL.get dType
                |> Maybe.withDefault 1
                |> (*) acc
    in
    L.foldl go 1 defenderTypes
