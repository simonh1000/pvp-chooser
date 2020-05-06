module HelperTests exposing (..)

import AssocList as Dict
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Helpers exposing (..)
import List as L
import Model exposing (..)
import Pokedex exposing (attacks, pokedex)
import Pokemon exposing (PType(..), effectiveness)
import Set
import Test exposing (..)


mkTeamsTests =
    describe "mkteams"
        [ test "3" <|
            \_ ->
                mkTeams [ 1, 2, 3 ]
                    |> Expect.equal [ ( 1, 2, 3 ) ]
        , test "4" <|
            \_ ->
                mkTeams [ 1, 2, 3, 4 ]
                    |> Expect.equal [ ( 1, 2, 3 ), ( 1, 2, 4 ), ( 1, 3, 4 ), ( 2, 3, 4 ) ]
        , test "5" <|
            \_ ->
                mkTeams [ 1, 2, 3, 4, 5 ]
                    |> Expect.equal [ ( 1, 2, 3 ), ( 1, 2, 4 ), ( 1, 2, 5 ), ( 1, 3, 4 ), ( 1, 3, 5 ), ( 1, 4, 5 ), ( 2, 3, 4 ), ( 2, 3, 5 ), ( 2, 4, 5 ), ( 3, 4, 5 ) ]
        ]



--


evaluateBattleTests =
    only <|
        describe "evaluateBattle"
            [ test "despite a good attack, azumarill repels a stunfisk much better" <|
                \_ ->
                    evaluateBattle pokedex attacks stunfisk "Azumarill"
                        |> Result.withDefault -1
                        |> Expect.within (Absolute 0.1) 0.8
            ]


evaluateAgainstOpponentTests =
    describe "evaluateAgainstOpponent"
        [ test "simple" <|
            \_ ->
                evaluateAgainstOpponent attacks azumarill [ Flying ]
                    |> Expect.within (Absolute 0.1) 1.3
        ]


calculateEffectivenessTests =
    describe "calculateEffectiveness"
        [ test "water -> water" <|
            \_ ->
                effectiveness
                    |> Dict.get Water
                    |> Maybe.map (calculateEffectiveness [ Water ])
                    |> Expect.equal (Just 0.625)
        , test "water -> water/steel" <|
            \_ ->
                effectiveness
                    |> Dict.get Water
                    |> Maybe.map (calculateEffectiveness [ Water, Steel ])
                    |> Expect.equal (Just 0.625)
        , test "water -> water/dragon" <|
            \_ ->
                effectiveness
                    |> Dict.get Water
                    |> Maybe.map (calculateEffectiveness [ Water, Dragon ])
                    |> Expect.equal (Just 0.390625)
        ]


evalScoreTests =
    describe "eval score"
        [ test "ferrothorn -> skarmory" <|
            \_ ->
                skarmory
                    |> Maybe.map .types
                    |> Maybe.map (evaluateAgainstOpponent attacks ferrothorn)
                    |> Expect.equal (Just 0.5078125)
        ]


azumarill : Pokemon
azumarill =
    Pokemon True "Azumarill" "Bubble" (Set.fromList [ "Hydro Pump", "Ice Beam" ]) Dict.empty


stunfisk : Pokemon
stunfisk =
    Pokemon True "Stunfisk" "Muddy Water" (Set.fromList [ "Thunder Shock" ]) Dict.empty


ferrothorn : Pokemon
ferrothorn =
    Pokemon True "Ferrothorn" "Metal Claw" (Set.fromList [ "Power Whip" ]) Dict.empty


skarmory =
    Dict.get "Skarmory" pokedex


getPokemon : String -> Maybe Pokemon
getPokemon name =
    pokedex
        |> Dict.get name
        |> Maybe.andThen
            (\p ->
                p.fast
                    |> L.head
                    |> Maybe.map (\fast -> Pokemon True name fast (Set.fromList p.charged) Dict.empty)
            )
