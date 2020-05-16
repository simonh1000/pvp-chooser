module Model exposing (..)

import Array exposing (Array)
import AssocList as Dict exposing (Dict)
import Autocomplete exposing (..)
import Common.CoreHelpers exposing (decodeSimpleCustomTypes)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Extra exposing (andMap)
import Json.Encode as Encode
import List as L
import Pokemon exposing (..)
import Set exposing (Set)


type alias Model =
    { season : Season
    , great : League
    , ultra : League
    , master : League
    , -- session data
      debug : Bool
    , page : Page
    , selectedPokemon : Maybe Pokemon
    , chooser : PokedexChooser
    , -- data
      pokedex : Dict String PokedexEntry -- name => meta
    , attacks : Dict String MoveType
    }


defaultModel : Model
defaultModel =
    { season = Great
    , great = blankLeague
    , ultra = blankLeague
    , master = blankLeague
    , debug = False
    , page = Choosing
    , selectedPokemon = Nothing
    , chooser = MyChooser "" Autocomplete.empty
    , pokedex = Dict.empty
    , attacks = Dict.empty
    }



-- Season


type Season
    = Great
    | Ultra
    | Master


decodeSeason : Decoder Season
decodeSeason =
    decodeSimpleCustomTypes
        [ ( "Great", Great )
        , ( "Ultra", Ultra )
        , ( "Master", Master )
        ]


encodeSeason : Season -> Value
encodeSeason s =
    Encode.string <|
        case s of
            Great ->
                "Great"

            Ultra ->
                "Ultra"

            Master ->
                "Master"



-- Page


type Page
    = Choosing
    | Battling
    | TeamOptions



-- -----------------------
-- Persistence
-- -----------------------


type alias Persisted =
    { season : Season
    , great : League
    , ultra : League
    , master : League
    }


decodePersisted : Decoder Persisted
decodePersisted =
    let
        dec s =
            Decode.field s decodeLeague
    in
    Decode.succeed Persisted
        |> andMap (Decode.oneOf [ Decode.field "season" decodeSeason, Decode.succeed Ultra ])
        |> andMap (dec "great")
        |> andMap (dec "ultra")
        |> andMap (dec "master")


encodePersisted : Model -> Encode.Value
encodePersisted model =
    Encode.object
        [ ( "season", encodeSeason model.season )
        , ( "great", encodeLeague model.great )
        , ( "ultra", encodeLeague model.ultra )
        , ( "master", encodeLeague model.master )
        ]



-- -----------------------
-- League
-- -----------------------


type alias League =
    { myPokemon : Array Pokemon
    , team : Team
    , opponents : List Opponent
    }


blankLeague =
    League Array.empty blankTeam []


encodeLeague : League -> Value
encodeLeague league =
    [ ( "myPokemon", Encode.list encodePokemon <| Array.toList league.myPokemon )
    , ( "team", encodeTeam league.team )
    , ( "opponents", Encode.list encodeOpponent league.opponents )
    ]
        |> Encode.object


decodeLeague : Decoder League
decodeLeague =
    Decode.succeed League
        |> andMap
            (Decode.oneOf
                [ Decode.field "myPokemon" <| Decode.list decodePokemon
                , Decode.succeed []
                ]
                |> Decode.map Array.fromList
            )
        |> andMap
            (Decode.oneOf
                [ Decode.field "team" decodeTeam
                , Decode.succeed blankTeam
                ]
            )
        |> andMap (Decode.field "opponents" <| Decode.list decodeOpponent)



-- -----------------------
-- Pokemon
-- -----------------------


type alias Pokemon =
    { expanded : Bool
    , name : String
    , fast : String -- the specific attack being used
    , charged : Set String
    , scores : Dict String Float -- opponent name => score
    }


blankPokemon : Pokemon
blankPokemon =
    Pokemon True "" "" Set.empty Dict.empty


decodePokemon : Decoder Pokemon
decodePokemon =
    Decode.succeed (\name fast charged -> Pokemon False name fast charged Dict.empty)
        |> andMap (Decode.field "name" Decode.string)
        |> andMap (Decode.field "fast" Decode.string)
        |> andMap (Decode.field "charged" <| decSet Decode.string)


encodePokemon : Pokemon -> Value
encodePokemon p =
    [ ( "name", Encode.string p.name )
    , ( "fast", Encode.string p.fast )
    , ( "charged", Encode.list Encode.string <| Set.toList p.charged )
    ]
        |> Encode.object



-- -----------------------
-- Opponent
-- -----------------------


type alias Opponent =
    { expanded : Bool
    , name : String
    , frequency : Int
    }


decodeOpponent : Decoder Opponent
decodeOpponent =
    Decode.map2 (Opponent False) (Decode.index 0 Decode.string) (Decode.index 1 Decode.int)


encodeOpponent : Opponent -> Value
encodeOpponent { name, frequency } =
    Encode.list identity [ Encode.string name, Encode.int frequency ]



-- -----------------------
-- PokedexEntry
-- -----------------------


type alias Pokedex =
    -- keyed on Name
    Dict String PokedexEntry


type alias PokedexEntry =
    { id : Int
    , types : List PType
    , fast : List String
    , charged : List String
    }


decodePokedex : Decoder (Dict String PokedexEntry)
decodePokedex =
    let
        dec =
            Decode.map2 Tuple.pair decodeName decodePokedexEntry
    in
    Decode.list dec
        |> Decode.map Dict.fromList


decodePokedexEntry : Decoder PokedexEntry
decodePokedexEntry =
    Decode.succeed PokedexEntry
        |> andMap (Decode.field "pokemon_id" Decode.int)
        |> andMap (Decode.field "type" <| Decode.list decodePType)
        |> andMap (Decode.field "fast_moves" <| Decode.list Decode.string)
        |> andMap (Decode.field "charged_moves" <| Decode.list Decode.string)


decodeName : Decoder String
decodeName =
    let
        mkName ( pName, pForm ) =
            if pForm == "Normal" then
                pName

            else
                pName ++ " - " ++ pForm
    in
    Decode.map2 Tuple.pair
        (Decode.field "pokemon_name" Decode.string)
        (Decode.oneOf [ Decode.field "form" Decode.string, Decode.succeed "Normal" ])
        |> Decode.map mkName



-- -----------------------
-- Chooser / Autocomplete
-- -----------------------


type PokedexChooser
    = MyChooser String Autocomplete.State
    | OpponentChooser String Autocomplete.State
    | NoChooser


mapSearch : (String -> String) -> PokedexChooser -> PokedexChooser
mapSearch fn chooser =
    case chooser of
        MyChooser string state ->
            MyChooser (fn string) state

        OpponentChooser string state ->
            OpponentChooser (fn string) state

        NoChooser ->
            NoChooser



--mapAutocomplete : (Autocomplete.State -> Autocomplete.State) -> PokedexChooser -> PokedexChooser
--mapAutocomplete fn chooser =
--    case chooser of
--        MyChooser string state ->
--            MyChooser string (fn state)
--
--        OpponentChooser string state ->
--            OpponentChooser string (fn state)
--
--        NoChooser ->
--            NoChooser
-- -----------------------
-- Team
-- -----------------------


type alias Team =
    { cand1 : Maybe String
    , cand2 : Maybe String
    , cand3 : Maybe String
    }


blankTeam =
    { cand1 = Nothing
    , cand2 = Nothing
    , cand3 = Nothing
    }


decodeTeam =
    Decode.map3 Team
        (Decode.maybe <| Decode.index 0 Decode.string)
        (Decode.maybe <| Decode.index 1 Decode.string)
        (Decode.maybe <| Decode.index 2 Decode.string)


encodeTeam t =
    [ t.cand1, t.cand2, t.cand3 ]
        |> L.filterMap identity
        |> Encode.list Encode.string



-- -----------------------
-- Effectiveness
-- -----------------------


type alias Effectiveness =
    Dict PType (Dict PType Float)


decodeEffectiveness : Decoder Effectiveness
decodeEffectiveness =
    decPTypeDict Decode.float
        |> decPTypeDict


decPTypeDict : Decoder b -> Decoder (Dict PType b)
decPTypeDict dec =
    Decode.keyValuePairs dec
        |> Decode.map
            (L.filterMap (\( k, v ) -> pTypeFromString k |> Maybe.map (\tp -> ( tp, v )))
                >> Dict.fromList
            )


getDefenceMeta : Effectiveness -> List PType -> Dict PType Float
getDefenceMeta effectiveness tps =
    let
        go _ dict =
            L.foldl (\tp acc -> Dict.get tp dict |> Maybe.withDefault 1 |> (*) acc) 1 tps
    in
    effectiveness
        |> Dict.map go



-- -----------------------
-- MoveType
-- -----------------------


type alias MoveType =
    { type_ : PType }


decodeMoves : Decoder (Dict String MoveType)
decodeMoves =
    Decode.list
        (Decode.map2 Tuple.pair
            (Decode.field "name" Decode.string)
            (Decode.map MoveType (Decode.field "type" decodePType))
        )
        |> Decode.map Dict.fromList



--


decSet : Decoder comparable -> Decoder (Set comparable)
decSet =
    Decode.list >> Decode.map Set.fromList
