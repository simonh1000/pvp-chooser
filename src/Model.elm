module Model exposing (..)

import AssocList as Dict exposing (Dict)
import Autocomplete exposing (..)
import Common.CoreHelpers exposing (decodeSimpleCustomTypes, exactMatchString, ifThenElse)
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
      page : Page
    , -- move to Page?
      selectedPokemon : Maybe String -- speciesId
    , chooser : SearchTool
    , errorMessage : Maybe String
    , debug : Bool
    , -- data
      pokedex : Dict String PokedexEntry -- name => meta
    , moves : Dict String MoveType
    }


defaultModel : Model
defaultModel =
    { season = Great
    , great = blankLeague
    , ultra = blankLeague
    , master = blankLeague
    , debug = False
    , page = Registering []
    , selectedPokemon = Nothing
    , chooser = MyChooser "" Autocomplete.empty
    , errorMessage = Nothing
    , pokedex = Dict.empty
    , moves = Dict.empty
    }


updateLeague : (League -> League) -> Model -> Model
updateLeague fn model =
    case model.season of
        Great ->
            { model | great = fn model.great }

        Ultra ->
            { model | ultra = fn model.ultra }

        Master ->
            { model | master = fn model.master }


getCurrentLeague : { a | season : Season, great : League, ultra : League, master : League } -> League
getCurrentLeague model =
    case model.season of
        Great ->
            model.great

        Ultra ->
            model.ultra

        Master ->
            model.master



-- -----------------------
-- Page
-- -----------------------


type Page
    = Intro
    | LoadingDex
    | Registering (List String)
    | TeamOptions
    | Battling
    | FatalError String


isRegistering : Page -> Bool
isRegistering page =
    case page of
        Registering _ ->
            True

        _ ->
            False



-- -----------------------
-- Persistence
-- -----------------------


type alias Persisted =
    { season : Maybe Season -- Nothing on first load
    , great : League
    , ultra : League
    , master : League
    }


decodePersisted : Decoder Persisted
decodePersisted =
    let
        dec s =
            Decode.oneOf
                [ Decode.field s decodeLeague
                , Decode.succeed blankLeague
                ]
    in
    Decode.succeed Persisted
        |> andMap (Decode.maybe <| Decode.field "season" decodeSeason)
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
-- Season
-- -----------------------


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



-- -----------------------
-- League
-- -----------------------


type alias League =
    { myPokemon : Dict String Pokemon -- speciesId => data
    , team : Team
    , opponents : Dict String Opponent -- name => opponent
    }


blankLeague : League
blankLeague =
    League Dict.empty blankTeam Dict.empty


encodeLeague : League -> Value
encodeLeague league =
    [ ( "myPokemon", encodeMyPokemon league.myPokemon )
    , ( "team", encodeTeam league.team )
    , ( "opponents", encodeOpponents league.opponents )
    ]
        |> Encode.object


decodeLeague : Decoder League
decodeLeague =
    Decode.succeed League
        |> andMap (Decode.field "myPokemon" <| decodeMyPokemon)
        |> andMap
            (Decode.oneOf
                [ Decode.field "team" decodeTeam
                , Decode.succeed blankTeam
                ]
            )
        |> andMap (Decode.field "opponents" decodeOpponents)



--decodeLeagueLegacy : Decoder League
--decodeLeagueLegacy =
--    let
--        mapTp tm =
--            case tm of
--                Pinned x ->
--                    Pinned <| convertNameToId x
--
--                Chosen x ->
--                    Chosen <| convertNameToId x
--
--                Unset ->
--                    Unset
--
--        mapOp ops =
--            let
--                go k v acc =
--                    Dict.insert (convertNameToId k) v acc
--            in
--            Dict.foldl go Dict.empty ops
--    in
--    Decode.succeed League
--        |> andMap
--            (Decode.oneOf
--                [ Decode.field "myPokemon" <| Decode.list decodeLegacyPokemon
--                , Decode.succeed []
--                ]
--                |> Decode.map Array.fromList
--            )
--        |> andMap
--            (Decode.oneOf
--                [ Decode.field "team" decodeTeam
--                , Decode.succeed blankTeam
--                ]
--                |> Decode.map (mapTeam mapTp)
--            )
--        |> andMap (Decode.field "opponents" decodeOpponents |> Decode.map mapOp)
-- -----------------------
-- Pokemon
-- -----------------------


type alias Pokemon =
    { expanded : Bool
    , fast : String -- the specific attack being used
    , charged : Set String
    , scores : Dict String Float -- opponent name => score
    }


blankPokemon : Pokemon
blankPokemon =
    Pokemon True "" Set.empty Dict.empty


decodeMyPokemon : Decoder (Dict String Pokemon)
decodeMyPokemon =
    Decode.map2 Tuple.pair (Decode.field "speciesId" Decode.string) decodePokemon
        |> Decode.list
        |> Decode.map Dict.fromList


decodePokemon : Decoder Pokemon
decodePokemon =
    Decode.succeed (\fast charged -> Pokemon False fast charged Dict.empty)
        |> andMap (Decode.field "fast" Decode.string)
        |> andMap (Decode.field "charged" <| decSet Decode.string)


encodeMyPokemon : Dict String Pokemon -> Value
encodeMyPokemon myPokemon =
    let
        enc speciesId p =
            Encode.object <| ( "speciesId", Encode.string speciesId ) :: encodePokemon p
    in
    myPokemon |> Dict.map enc |> Dict.values |> Encode.list identity


encodePokemon : Pokemon -> List ( String, Value )
encodePokemon p =
    [ ( "fast", Encode.string p.fast )
    , ( "charged", Encode.list Encode.string <| Set.toList p.charged )
    ]



-- -----------------------
-- Team
-- -----------------------


type alias Team =
    { cand1 : TeamMember
    , cand2 : TeamMember
    , cand3 : TeamMember
    }


blankTeam : Team
blankTeam =
    { cand1 = Unset
    , cand2 = Unset
    , cand3 = Unset
    }


mapTeam : (TeamMember -> TeamMember) -> Team -> Team
mapTeam fn team =
    { cand1 = fn team.cand1
    , cand2 = fn team.cand2
    , cand3 = fn team.cand3
    }


copyPinning : Team -> ( String, String, String ) -> Team
copyPinning currTeam ( p1, p2, p3 ) =
    let
        convertor : String -> TeamMember
        convertor p =
            if L.member (Pinned p) (mkTeamList currTeam) then
                Pinned p

            else
                Chosen p
    in
    { cand1 = convertor p1
    , cand2 = convertor p2
    , cand3 = convertor p3
    }


hasMember : String -> Team -> Bool
hasMember tgt =
    L.any (eqMember tgt) << mkTeamList


mkTeamList : Team -> List TeamMember
mkTeamList t =
    [ t.cand1, t.cand2, t.cand3 ]


removeFromTeam : String -> Team -> Team
removeFromTeam name team =
    { cand1 = ifThenElse (eqMember name team.cand1) Unset team.cand1
    , cand2 = ifThenElse (eqMember name team.cand2) Unset team.cand2
    , cand3 = ifThenElse (eqMember name team.cand3) Unset team.cand3
    }


decodeTeam : Decoder Team
decodeTeam =
    Decode.map3 Team
        (Decode.index 0 decodeTM)
        (Decode.index 1 decodeTM)
        (Decode.index 2 decodeTM)


encodeTeam : Team -> Value
encodeTeam t =
    [ t.cand1, t.cand2, t.cand3 ]
        |> Encode.list encodeTM



-- -----------------------
-- TeamMember
-- -----------------------


type TeamMember
    = Unset
    | Chosen String
    | Pinned String


getPinnedMember : TeamMember -> Maybe String
getPinnedMember teamMember =
    case teamMember of
        Pinned name ->
            Just name

        _ ->
            Nothing


getMember : TeamMember -> Maybe String
getMember teamMember =
    case teamMember of
        Unset ->
            Nothing

        Chosen name ->
            Just name

        Pinned name ->
            Just name


eqMember : String -> TeamMember -> Bool
eqMember tgt teamMember =
    case teamMember of
        Unset ->
            False

        Chosen m ->
            tgt == m

        Pinned m ->
            tgt == m


togglePinning : String -> TeamMember -> TeamMember
togglePinning tgt teamMember =
    case teamMember of
        Unset ->
            Unset

        Chosen name ->
            ifThenElse (name == tgt) (Pinned name) teamMember

        Pinned name ->
            ifThenElse (name == tgt) (Chosen name) teamMember


decodeTM : Decoder TeamMember
decodeTM =
    Decode.oneOf
        [ exactMatchString Decode.string "Unset" (Decode.succeed Unset)
        , exactMatchString (Decode.index 0 Decode.string) "Chosen" (Decode.map Chosen <| Decode.index 1 Decode.string)
        , exactMatchString (Decode.index 0 Decode.string) "Pinned" (Decode.map Pinned <| Decode.index 1 Decode.string)
        ]


encodeTM : TeamMember -> Value
encodeTM teamMember =
    case teamMember of
        Unset ->
            Encode.string "Unset"

        Chosen m ->
            Encode.list Encode.string [ "Chosen", m ]

        Pinned m ->
            Encode.list Encode.string [ "Pinned", m ]



-- -----------------------
-- Opponent
-- -----------------------


type alias Opponent =
    { expanded : Bool
    , frequency : Int
    }


decodeOpponents : Decoder (Dict String Opponent)
decodeOpponents =
    Decode.list decodeOpponent
        |> Decode.map Dict.fromList


encodeOpponents : Dict String Opponent -> Value
encodeOpponents opponents =
    opponents
        |> Dict.toList
        |> L.map encodeOpponent
        |> Encode.list identity


decodeOpponent : Decoder ( String, Opponent )
decodeOpponent =
    Decode.map2 Tuple.pair
        (Decode.index 0 Decode.string)
        (Decode.map (Opponent False) <| Decode.index 1 Decode.int)


encodeOpponent : ( String, Opponent ) -> Value
encodeOpponent ( name, { frequency } ) =
    Encode.list identity [ Encode.string name, Encode.int frequency ]



-- -----------------------
-- Gamemaster: Pokedex
-- -----------------------


type alias Pokedex =
    -- keyed on Name
    Dict String PokedexEntry


attachRankings : Dict String RankingEntry -> Pokedex -> Pokedex
attachRankings rankings =
    Dict.map
        (\speciesId entry ->
            case Dict.get speciesId rankings of
                Just r ->
                    { entry | recMoves = r.moveSet, score = Just r.score }

                Nothing ->
                    resetRanking entry
        )


resetPokedex : Pokedex -> Pokedex
resetPokedex =
    Dict.map (\_ -> resetRanking)


resetRanking : PokedexEntry -> PokedexEntry
resetRanking entry =
    { entry | recMoves = [], score = Nothing }


type alias PokedexEntry =
    { speciesName : String
    , types : List PType
    , fast : List String
    , charged : List String
    , recMoves : List String
    , score : Maybe Float
    }


blankDex : PokedexEntry
blankDex =
    { speciesName = ""
    , types = []
    , fast = []
    , charged = []
    , recMoves = []
    , score = Nothing
    }


decodePokedex : Decoder (Dict String PokedexEntry)
decodePokedex =
    Decode.map2 Tuple.pair (Decode.field "speciesId" Decode.string) decodePokedexEntry
        |> Decode.list
        |> Decode.map Dict.fromList


decodePokedexEntry : Decoder PokedexEntry
decodePokedexEntry =
    Decode.succeed (\speciesName types fast charged -> { blankDex | speciesName = speciesName, types = types, fast = fast, charged = charged })
        |> andMap (Decode.field "speciesName" Decode.string)
        |> andMap (Decode.field "types" decodeTypes)
        |> andMap (Decode.field "fastMoves" <| Decode.list Decode.string)
        |> andMap (Decode.field "chargedMoves" <| Decode.list Decode.string)



-- -----------------------
-- Gamemaster: Moves
-- -----------------------


type alias MoveType =
    { name : String
    , type_ : PType
    }


decodeMoves : Decoder (Dict String MoveType)
decodeMoves =
    Decode.map2 Tuple.pair
        (Decode.field "moveId" Decode.string)
        decMoveType
        |> Decode.list
        |> Decode.map Dict.fromList


decMoveType : Decoder MoveType
decMoveType =
    Decode.map2 MoveType
        (Decode.field "name" Decode.string)
        (Decode.field "type" decodePType)



-- -----------------------
-- Rankings
-- -----------------------


type alias RankingEntry =
    { moveSet : List String
    , score : Float
    }


decodeRankings : Decoder (Dict String RankingEntry)
decodeRankings =
    Decode.map2 Tuple.pair (Decode.field "speciesId" Decode.string) decodeRankingEntry
        |> Decode.list
        |> Decode.map Dict.fromList


decodeRankingEntry : Decoder RankingEntry
decodeRankingEntry =
    Decode.succeed RankingEntry
        |> andMap (Decode.field "moveset" <| Decode.list Decode.string)
        |> andMap (Decode.field "score" Decode.float)



-- -----------------------
-- Chooser / Autocomplete
-- -----------------------


type SearchTool
    = MyChooser String Autocomplete.State
    | OpponentChooser String Autocomplete.State
    | NoChooser


resetSearch : SearchTool -> SearchTool
resetSearch =
    mapSearch (\_ -> "") >> mapAutocomplete (\_ -> Autocomplete.empty)


mapSearch : (String -> String) -> SearchTool -> SearchTool
mapSearch fn chooser =
    case chooser of
        MyChooser string state ->
            MyChooser (fn string) state

        OpponentChooser string state ->
            OpponentChooser (fn string) state

        NoChooser ->
            NoChooser


mapAutocomplete : (Autocomplete.State -> Autocomplete.State) -> SearchTool -> SearchTool
mapAutocomplete fn chooser =
    case chooser of
        MyChooser string state ->
            MyChooser string (fn state)

        OpponentChooser string state ->
            OpponentChooser string (fn state)

        NoChooser ->
            NoChooser



-- Helpers


decSet : Decoder comparable -> Decoder (Set comparable)
decSet =
    Decode.list >> Decode.map Set.fromList



--decodeName : Decoder String
--decodeName =
--    let
--        mkName ( pName, pForm ) =
--            if pForm == "Normal" then
--                pName
--
--            else
--                pName ++ " - " ++ pForm
--    in
--    Decode.map2 Tuple.pair
--        (Decode.field "pokemon_name" Decode.string)
--        (Decode.oneOf [ Decode.field "form" Decode.string, Decode.succeed "Normal" ])
--        |> Decode.map mkName
