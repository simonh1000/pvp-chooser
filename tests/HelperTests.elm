module HelperTests exposing (..)

import AssocList as Dict
import Expect exposing (Expectation)
import Helpers exposing (..)
import List as L
import Model exposing (..)
import Pokedex exposing (attacks, pokedex)
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
                    |> Expect.equal [ ( 1, 2, 3 ), ( 1, 2, 4 ), ( 2, 3, 4 ) ]
        , test "5" <|
            \_ ->
                mkTeams [ 1, 2, 3, 4, 5 ]
                    |> Expect.equal [ ( 1, 2, 3 ), ( 1, 2, 4 ), ( 1, 2, 5 ), ( 1, 3, 4 ), ( 1, 3, 5 ), ( 1, 4, 5 ), ( 2, 3, 4 ), ( 2, 3, 5 ), ( 2, 4, 5 ), ( 3, 4, 5 ) ]
        ]



--


evalScoreTests =
    describe "eval score"
        [ test "ferrothorn -> skarmory" <|
            \_ ->
                skarmory
                    |> Maybe.map .types
                    |> Maybe.map (evaluateAgainstOpponent model ferrothorn)
                    |> Expect.equal (Just 0.5078125)
        ]


ferrothorn : Pokemon
ferrothorn =
    Pokemon True "Ferrothorn" "Metal Claw" (Set.fromList [ "Power Whip" ]) Dict.empty


skarmory =
    Dict.get "Skarmory" pokedex


model =
    { defaultModel
        | pokedex = pokedex
        , attacks = attacks
    }


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
