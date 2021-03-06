module HelperTests exposing (..)

import AssocList as AL
import Dict
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Helpers exposing (..)
import List as L
import Model exposing (..)
import Pokedex exposing (attacks, pokedex)
import Pokemon exposing (PType(..), effectiveness)
import Set
import Test exposing (..)


mkTeamsTests : Test
mkTeamsTests =
    describe "mkteams"
        [ test "3" <|
            \_ ->
                mkTeams3 [ 1, 2, 3 ]
                    |> Expect.equal [ ( 1, 2, 3 ) ]
        , test "4" <|
            \_ ->
                mkTeams3 [ 1, 2, 3, 4 ]
                    |> Expect.equal [ ( 1, 2, 3 ), ( 1, 2, 4 ), ( 1, 3, 4 ), ( 2, 3, 4 ) ]
        , test "5" <|
            \_ ->
                mkTeams3 [ 1, 2, 3, 4, 5 ]
                    |> Expect.equal [ ( 1, 2, 3 ), ( 1, 2, 4 ), ( 1, 2, 5 ), ( 1, 3, 4 ), ( 1, 3, 5 ), ( 1, 4, 5 ), ( 2, 3, 4 ), ( 2, 3, 5 ), ( 2, 4, 5 ), ( 3, 4, 5 ) ]
        ]



--


evaluateBattleTests =
    describe "evaluateBattle"
        [ test "stunfisk vs Azumarill" <|
            \_ ->
                evaluateAgainstOpponent attacks stunfisk [ Water, Fairy ]
                    |> Expect.equal ( "THUNDER_SHOCK, DISCHARGE", 1.6 )
        , test "Azu vs stunfisk" <|
            \_ ->
                evaluateOpponentAttacks attacks azuEntry stunfiskEntry.types
                    |> Expect.within (Absolute 0.1) 1.36
        , test "despite a good attack, azumarill repels a stunfisk much better" <|
            \_ ->
                evaluateBattle pokedex attacks "stunfisk" stunfisk "azumarill"
                    |> Result.withDefault -1
                    |> Expect.within (Absolute 0.1) 0.8
        ]


evaluateAgainstOpponentTests =
    describe "evaluateAgainstOpponent"
        [ test "azumarill vs flyer" <|
            \_ ->
                evaluateAgainstOpponent attacks azumarill [ Flying ]
                    |> Tuple.second
                    |> Expect.within (Absolute 0.1) 1.3
        ]


evalScoreTests =
    describe "eval score"
        [ test "ferrothorn (bullet seed, thunder) -> skarmory" <|
            \_ ->
                skarmory
                    |> Maybe.map .types
                    |> Maybe.map (evaluateAgainstOpponent attacks ferrothorn)
                    |> Expect.equal (Just ( "BULLET_SEED, THUNDER", 0.9953125 ))
        ]


calculateEffectivenessTests =
    describe "calculateEffectiveness"
        [ test "water -> water" <|
            \_ ->
                effectiveness
                    |> AL.get Water
                    |> Maybe.map (calculateEffectiveness [ Water ])
                    |> Expect.equal (Just 0.625)
        , test "water -> water/steel" <|
            \_ ->
                effectiveness
                    |> AL.get Water
                    |> Maybe.map (calculateEffectiveness [ Water, Steel ])
                    |> Expect.equal (Just 0.625)
        , test "water -> water/dragon" <|
            \_ ->
                effectiveness
                    |> AL.get Water
                    |> Maybe.map (calculateEffectiveness [ Water, Dragon ])
                    |> Expect.equal (Just 0.390625)
        , test "fighter -> alolan marowak" <|
            \_ ->
                effectiveness
                    |> AL.get Fighting
                    |> Maybe.map (calculateEffectiveness [ Fire, Ghost ])
                    |> Expect.equal (Just 0.390625)
        ]


azumarill : Pokemon
azumarill =
    getPokemon "azumarill"


azuEntry =
    Dict.get "azumarill" pokedex |> fromJust


stunfisk : Pokemon
stunfisk =
    getPokemon "stunfisk"


stunfiskEntry =
    Dict.get "stunfisk" pokedex |> fromJust


ferrothorn : Pokemon
ferrothorn =
    getPokemon "ferrothorn"


skarmory : Maybe PokedexEntry
skarmory =
    Dict.get "skarmory" pokedex


fromJust mb =
    case mb of
        Just m ->
            m

        Nothing ->
            Debug.todo "missing"


getPokemon : String -> Pokemon
getPokemon name =
    pokedex
        |> Dict.get name
        |> Maybe.andThen
            (\p ->
                p.fast
                    |> L.head
                    |> Maybe.map (\fast -> Pokemon True fast (Set.fromList p.charged) Dict.empty)
            )
        |> fromJust
