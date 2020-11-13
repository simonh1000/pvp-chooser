module Model exposing (..)

import Autocomplete exposing (..)
import Common.CoreHelpers exposing (decodeSimpleCustomTypes, exactMatchString, ifThenElse)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Extra as DE exposing (andMap)
import Json.Encode as Encode
import List as L
import Pokemon exposing (..)
import Set exposing (Set)


type alias Model =
    { season : SeasonName
    , leagues : Dict String League
    , -- session data
      page : Page
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
    , leagues = Dict.empty
    , debug = False
    , page = LoadingDex
    , chooser = MyChooser ""
    , errorMessage = Nothing
    , pokedex = Dict.empty
    , moves = Dict.empty
    }


updateLeague : (League -> League) -> Model -> Model
updateLeague fn model =
    let
        updater =
            Maybe.withDefault blankLeague >> fn >> Just
    in
    { model | leagues = Dict.update (serialiseSeason model.season) updater model.leagues }


getCurrentLeague : Model -> League
getCurrentLeague model =
    Dict.get (serialiseSeason model.season) model.leagues
        |> Maybe.withDefault blankLeague



-- -----------------------
-- Page
-- -----------------------


type Page
    = Intro
    | LoadingDex
    | Registering RegisteringModel
    | TeamOptions String -- search term
    | Battling
    | FatalError String


mapRegistering : (RegisteringModel -> RegisteringModel) -> Page -> Page
mapRegistering function page =
    case page of
        Registering m ->
            Registering <| function m

        _ ->
            page


type alias RegisteringModel =
    { opponents : List String
    , selectedPokemon : Maybe String -- speciesId
    }


registerPage =
    Registering blankRegistering


blankRegistering : RegisteringModel
blankRegistering =
    { opponents = [], selectedPokemon = Nothing }


isRegistering : Page -> Bool
isRegistering page =
    case page of
        Registering _ ->
            True

        _ ->
            False


isTeamOptions : Page -> Bool
isTeamOptions page =
    case page of
        TeamOptions _ ->
            True

        _ ->
            False



-- -----------------------
-- Persistence
-- -----------------------


type alias Persisted =
    { season : SeasonName -- Nothing on first load
    , leagues : Dict String League
    }


decodePersisted : Decoder Persisted
decodePersisted =
    Decode.succeed Persisted
        |> andMap (DE.withDefault Ultra <| Decode.field "season" decodeSeason)
        |> andMap (DE.withDefault Dict.empty decodeLeagues)


encodePersisted : Model -> Encode.Value
encodePersisted model =
    model.leagues
        |> Dict.toList
        |> L.map (Tuple.mapSecond encodeLeague)
        |> (::) ( "season", encodeSeason model.season )
        |> Encode.object



-- -----------------------
-- Season
-- -----------------------


type alias Season =
    { name : SeasonName
    , serialised : String
    , url : String
    , pretty : String
    , stringified : String
    }

seasonsData : List Season
seasonsData =
    [ Season LittleCup "little" "little/rankings-500.json" "Little Cup" "little"
    , greatSeason
    , Season Kanto "kanto" "kanto/rankings-1500.json" "Kanto Cup" "kanto"
    , Season UltraPremier "ultra-premier" "premier/rankings-2500.json" "Ultra: Premier Cup" "UltraPremier"
    , Season Ultra "ultra" "all/rankings-2500.json" "Ultra League" "Ultra"
    , Season MasterPremier "premier" "premier/rankings-10000.json" "Master: Premier Cup" "Premier"
    , Season Master "master" "all/rankings-10000.json" "Master League" "Master"
    ]


greatSeason : Season
greatSeason =
    Season Great "great" "all/rankings-1500.json" "Great League" "Great"

type SeasonName
    = LittleCup
    | Great
    | Kanto
    | UltraPremier
    | Ultra
    | MasterPremier
    | Master


decodeSeason : Decoder SeasonName
decodeSeason =
    let
        convert season =
            ( stringFromSeason season, season )
    in
    seasons |> L.map convert |> decodeSimpleCustomTypes


encodeSeason : SeasonName -> Value
encodeSeason =
    Encode.string << stringFromSeason



seasons : List SeasonName
seasons =
    L.map .name seasonsData



stringFromSeason : SeasonName -> String
stringFromSeason =
    getSeason >> .stringified


ppSeason : SeasonName -> String
ppSeason =
    getSeason >> .pretty


getUrl : SeasonName -> String
getUrl =
    getSeason >> .url


serialiseSeason : SeasonName -> String
serialiseSeason =
    getSeason >> .serialised


deserialiseSeason : String -> SeasonName
deserialiseSeason tgt =
    L.foldl (\season acc -> ifThenElse (season.serialised == tgt) season.name acc) Master seasonsData


getSeason : SeasonName -> Season
getSeason seasonName =
    L.foldl (\season acc -> ifThenElse (season.name == seasonName) season acc) greatSeason seasonsData



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
    { myPokemon = Dict.empty
    , team = blankTeam
    , opponents = Dict.empty
    }


decodeLeagues =
    Decode.keyValuePairs (Decode.maybe decodeLeague)
        |> Decode.map (L.filterMap (\( k, v ) -> Maybe.map (Tuple.pair k) v) >> Dict.fromList)


decodeLeague : Decoder League
decodeLeague =
    Decode.succeed League
        |> andMap (DE.withDefault Dict.empty <| Decode.field "myPokemon" <| decodeMyPokemon)
        |> andMap (DE.withDefault blankTeam <| Decode.field "team" decodeTeam)
        |> andMap (Decode.field "opponents" decodeOpponents)


encodeLeague : League -> Value
encodeLeague league =
    [ ( "myPokemon", encodeMyPokemon league.myPokemon )
    , ( "team", encodeTeam league.team )
    , ( "opponents", encodeOpponents league.opponents )
    ]
        |> Encode.object



-- -----------------------
-- Pokemon
-- -----------------------


type alias Pokemon =
    { expanded : Bool
    , fast : String -- the specific attack being used
    , charged : Set String
    , scores : Dict String Float -- opponent name => score
    }


toggleCharged : String -> Pokemon -> Pokemon
toggleCharged move pokemon =
    let
        updater charged =
            if Set.member move charged then
                Set.remove move charged

            else
                Set.insert move charged
    in
    { pokemon | charged = updater pokemon.charged }


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


addToTeam : TeamMember -> Team -> Team
addToTeam teamMember team =
    if team.cand1 == Unset then
        { team | cand1 = teamMember }

    else if team.cand2 == Unset then
        { team | cand2 = teamMember }

    else if team.cand3 == Unset then
        { team | cand3 = teamMember }

    else
        team


getPinnedTeam : Team -> Team
getPinnedTeam team =
    let
        mapper member =
            case member of
                Pinned _ ->
                    member

                _ ->
                    Unset
    in
    mapTeam mapper team


mapTeam : (TeamMember -> TeamMember) -> Team -> Team
mapTeam fn team =
    { cand1 = fn team.cand1
    , cand2 = fn team.cand2
    , cand3 = fn team.cand3
    }


mkTeamList : Team -> List TeamMember
mkTeamList t =
    [ t.cand1, t.cand2, t.cand3 ]


getTeamList =
    mkTeamList >> L.filterMap extractSpeciesId >> L.sort


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


extractSpeciesId : TeamMember -> Maybe String
extractSpeciesId teamMember =
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


blankOpponent =
    { expanded = False
    , frequency = 1
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



-- PokedexEntry


type alias PokedexEntry =
    { speciesName : String
    , types : List PType
    , fast : List String
    , charged : List String
    , elite : List String -- legacy moves
    , tags : Set String
    , -- from rankings
      recMoves : List String
    , score : Maybe Float
    }


resetRanking : PokedexEntry -> PokedexEntry
resetRanking entry =
    { entry | recMoves = [], score = Nothing }


mkDexEntry : String -> List PType -> List String -> List String -> List String -> Set String -> PokedexEntry
mkDexEntry speciesName types fast charged elite tags =
    { speciesName = speciesName
    , types = types
    , fast = fast
    , charged = charged
    , elite = elite
    , tags = tags
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
    Decode.succeed mkDexEntry
        |> andMap (Decode.field "speciesName" Decode.string)
        |> andMap (Decode.field "types" decodeTypes)
        |> andMap (Decode.field "fastMoves" <| Decode.list Decode.string)
        |> andMap (Decode.field "chargedMoves" <| Decode.list Decode.string)
        |> andMap (DE.withDefault [] <| Decode.field "eliteMoves" <| Decode.list Decode.string)
        |> andMap (DE.withDefault Set.empty <| Decode.field "tags" <| decSet Decode.string)



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
    = MyChooser String
    | OpponentChooser String Autocomplete.State
    | OptionsFilter String -- Team Options page
    | NoChooser


resetSearch : SearchTool -> SearchTool
resetSearch =
    mapSearch (\_ -> "") >> mapAutocomplete (\_ -> Autocomplete.empty)


mapSearch : (String -> String) -> SearchTool -> SearchTool
mapSearch fn chooser =
    case chooser of
        MyChooser string ->
            MyChooser (fn string)

        OpponentChooser string state ->
            OpponentChooser (fn string) state

        OptionsFilter string ->
            OptionsFilter (fn string)

        NoChooser ->
            NoChooser


mapAutocomplete : (Autocomplete.State -> Autocomplete.State) -> SearchTool -> SearchTool
mapAutocomplete fn chooser =
    case chooser of
        OpponentChooser string state ->
            OpponentChooser string (fn state)

        _ ->
            chooser



-- Helpers


decSet : Decoder comparable -> Decoder (Set comparable)
decSet =
    Decode.list >> Decode.map Set.fromList
