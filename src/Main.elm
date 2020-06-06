module Main exposing (main)

import Array exposing (Array)
import Array.Extra as AE
import AssocList as Dict exposing (Dict)
import Autocomplete exposing (..)
import Browser
import Common.CoreHelpers exposing (ifThenElse)
import FormatNumber
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import Helpers exposing (addScoresToLeague, calculateEffectiveness, evaluateTeam, lookupName)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Extra exposing (andMap)
import List as L
import List.Extra as LE
import Model exposing (..)
import Pokemon exposing (Effectiveness, PType, effectiveness, stringFromPType)
import Ports
import Result.Extra as RE
import Set


init : Value -> ( Model, Cmd Msg )
init value =
    case Decode.decodeValue decodeFlags value of
        Ok flags ->
            let
                model =
                    { defaultModel
                        | season = Maybe.withDefault Ultra flags.persisted.season
                        , great = flags.persisted.great
                        , ultra = flags.persisted.ultra
                        , master = flags.persisted.master
                        , pokedex = flags.pokedex
                        , attacks = flags.moves
                        , rankings2500 = flags.rankings2500
                        , debug = flags.debug
                    }

                page =
                    case flags.persisted.season of
                        Just _ ->
                            model
                                |> getCurrentLeague
                                |> .opponents
                                |> sortOpponents
                                |> Registering

                        Nothing ->
                            Intro
            in
            ( addScores { model | page = page }, Cmd.none )

        Err err ->
            ( { defaultModel | page = FatalError <| Decode.errorToString err }, Cmd.none )


type alias Flags =
    { persisted : Persisted
    , --gamemaster
      pokedex : Dict String PokedexEntry -- keys by lowercase name
    , moves : Dict String MoveType
    , rankings2500 : Dict String RankingEntry
    , debug : Bool
    }


decodeFlags : Decoder Flags
decodeFlags =
    Decode.succeed Flags
        |> andMap (Decode.field "myPokemon" decodePersisted)
        |> andMap (Decode.field "pokemon" decodePokedex)
        |> andMap (Decode.field "moves" decodeMoves)
        |> andMap (Decode.field "rankings2500" decodeRankings)
        |> andMap (Decode.field "debug" Decode.bool)



-- ----------------------------------------
-- UPDATE
-- ----------------------------------------


type Msg
    = SwitchPage Page
    | SwitchSeason Season
      -- Autocomplete
    | ACMsg Autocomplete.Msg
    | ACSearch String
    | ACSelect Bool String -- isMyPokemon name
    | SetAutoComplete PokedexChooser
      -- My pokemons
    | ToggleMyPokemon Int
    | SelectFastMove Int String
    | SelectChargedMove Int String
    | RemovePokemon Int
    | PinTeamMember String
      -- Team chooser
    | SetTeam ( String, String, String )
      -- middle
    | SelectCandidate Pokemon -- first part of adding to team
    | UpdateTeam Team -- second part
      -- My opponents
    | ToggleOpponent String
    | UpdateOpponentFrequency String Int -- name
    | RemoveOpponent String


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        SwitchPage page ->
            ( { model | page = page }, Cmd.none )

        SwitchSeason season ->
            let
                newModel =
                    { model | season = season }

                newPage =
                    case model.page of
                        Registering _ ->
                            newModel
                                |> getCurrentLeague
                                |> .opponents
                                |> sortOpponents
                                |> Registering

                        p ->
                            p
            in
            { newModel | page = newPage } |> andPersist

        -- Autocomplete
        ACMsg msg ->
            let
                handler isMyPokemon search autocomplete =
                    getRelevantChoices search model.pokedex
                        |> Autocomplete.update (updateConfig isMyPokemon) msg autocomplete

                ( chooser, c ) =
                    case model.chooser of
                        MyChooser search autocomplete ->
                            handler True search autocomplete
                                |> Tuple.mapFirst (MyChooser search)

                        OpponentChooser search autocomplete ->
                            handler False search autocomplete
                                |> Tuple.mapFirst (OpponentChooser search)

                        NoChooser ->
                            ( NoChooser, Nothing )

                model_ =
                    { model | chooser = chooser }
            in
            case c of
                Just newMsg ->
                    update newMsg model_

                Nothing ->
                    ( model_, Cmd.none )

        ACSearch search ->
            ( { model | chooser = mapSearch (\_ -> search) model.chooser }, Cmd.none )

        ACSelect isMyPokemon speciesId ->
            let
                updater l =
                    if isMyPokemon then
                        let
                            pokemon =
                                { blankPokemon
                                    | speciesId = speciesId
                                    , fast = model.pokedex |> Dict.get speciesId |> Maybe.andThen (\{ fast } -> L.head fast) |> Maybe.withDefault ""
                                    , charged = Set.empty
                                }
                        in
                        { l | myPokemon = Array.push pokemon l.myPokemon }

                    else
                        { l | opponents = Dict.insert speciesId (Opponent False 1) l.opponents }

                newModel =
                    updateLeague updater model

                page =
                    case model.page of
                        Registering lst ->
                            if isMyPokemon then
                                Registering lst

                            else
                                Registering <| lst ++ [ speciesId ]

                        p ->
                            p
            in
            { newModel
                | chooser = mapSearch (\_ -> "") model.chooser
                , page = page
            }
                |> andPersist

        SetAutoComplete chooser ->
            ( { model | chooser = chooser }, Cmd.none )

        -- My Pokemones
        ToggleMyPokemon idx ->
            model
                |> updateLeague (\l -> { l | myPokemon = AE.update idx (\p -> { p | expanded = not p.expanded }) l.myPokemon })
                |> andPersist

        SelectFastMove idx move ->
            model
                |> updateLeague (\l -> { l | myPokemon = AE.update idx (\p -> { p | fast = move }) l.myPokemon })
                |> andPersist

        SelectChargedMove idx move ->
            let
                updater charged =
                    if Set.member move charged then
                        Set.remove move charged

                    else
                        Set.insert move charged
            in
            model
                |> updateLeague (\l -> { l | myPokemon = AE.update idx (\p -> { p | charged = updater p.charged }) l.myPokemon })
                |> andPersist

        RemovePokemon idx ->
            let
                updater l =
                    case Array.get idx l.myPokemon of
                        Just p ->
                            { l
                                | myPokemon = AE.removeAt idx l.myPokemon
                                , team = removeFromTeam p.speciesId l.team
                            }

                        Nothing ->
                            { l | myPokemon = AE.removeAt idx l.myPokemon }
            in
            model
                |> updateLeague updater
                |> andPersist

        PinTeamMember name ->
            let
                updater l =
                    { l
                        | team =
                            { cand1 = togglePinning name l.team.cand1
                            , cand2 = togglePinning name l.team.cand2
                            , cand3 = togglePinning name l.team.cand3
                            }
                    }
            in
            model
                |> updateLeague updater
                |> andPersist

        -- team chooser
        SetTeam team ->
            let
                updater l =
                    { l | team = copyPinning l.team team }
            in
            model
                |> updateLeague updater
                |> andPersist

        SelectCandidate pokemon ->
            ( { model
                | selectedPokemon =
                    if Just pokemon == model.selectedPokemon then
                        Nothing

                    else
                        Just pokemon
              }
            , Cmd.none
            )

        UpdateTeam team ->
            { model | selectedPokemon = Nothing }
                |> updateLeague (\l -> { l | team = team })
                |> andPersist

        ToggleOpponent name ->
            let
                mapper n op =
                    ifThenElse (n == name) { op | expanded = not op.expanded } op
            in
            ( updateLeague (\l -> { l | opponents = Dict.map mapper l.opponents }) model
            , Cmd.none
            )

        UpdateOpponentFrequency name y ->
            model
                |> updateLeague (\l -> { l | opponents = Dict.map (\n op -> ifThenElse (n == name) { op | frequency = op.frequency + y } op) l.opponents })
                |> andPersist

        RemoveOpponent name ->
            model
                |> updateLeague (\l -> { l | opponents = Dict.filter (\n _ -> n /= name) l.opponents })
                |> andPersist


addScores : Model -> Model
addScores model =
    { model
        | great = addScoresToLeague model model.great
        , ultra = addScoresToLeague model model.ultra
        , master = addScoresToLeague model model.master
    }


andPersist : Model -> ( Model, Cmd msg )
andPersist model =
    ( addScores model
    , Ports.toJs { tag = "Persist", payload = encodePersisted model }
    )


updateConfig : Bool -> UpdateConfig Msg ( String, PokedexEntry )
updateConfig isMyPokemon =
    { onKeyDown =
        \code maybeId ->
            if code == Enter then
                Maybe.map (ACSelect isMyPokemon) maybeId

            else if code == Esc then
                Just <| ACSearch ""

            else
                Nothing
    , onInput = ACSearch
    , onMouseEnter = \_ -> Nothing
    , onMouseLeave = \_ -> Nothing
    , onMouseClick = Just << ACSelect isMyPokemon
    , onTooLow = Nothing
    , onTooHigh = Nothing
    , toId = toId
    , separateSelections = False
    }


getRelevantChoices : String -> Dict String PokedexEntry -> List ( String, PokedexEntry )
getRelevantChoices search pokedex =
    let
        search_ =
            String.toLower search
    in
    pokedex
        --|> Dict.keys
        |> Dict.filter (\_ { speciesName } -> String.contains search_ (String.toLower speciesName))
        |> Dict.toList


toId : ( String, PokedexEntry ) -> String
toId =
    Tuple.first



-- ----------------------------------------
-- VIEW
-- ----------------------------------------


view : Model -> Html Msg
view model =
    let
        cls s =
            class <| "p-2 main flex-grow flex flex-row " ++ s

        league =
            case model.season of
                Great ->
                    model.great

                Ultra ->
                    model.ultra

                Master ->
                    model.master

        lst =
            sortOpponents league.opponents
    in
    div [ class "h-screen flex flex-col" ]
        [ pvpHeader lst model.page
        , case model.page of
            Intro ->
                div [ class "intro flex-grow p-3" ]
                    [ h2 [] [ text "Introduction" ]
                    , p [] [ text "This app will help you keep track of your pokemon and their types as well as of the competitors you encounter. The data you add is stored on your computer and no where else." ]
                    , img [ src "images/screenshot.png", class "mb-2" ] []
                    , p [] [ text "To start, you add your pokemon for each league, and those that you encounter in combat. You select you pokemons' attacks, and the PvPoke recommendations are shown in line (Ultra League only at present)." ]
                    , p [] [ text "The app enables you to build teams of three and to compare them. Each team gets a score. The absolute value is meaningless, but the relative scores may help you. The algorithm focuses on type dominance and does not take into account the details of energy generation and usage. It is also unlikely to recommend an unbalanced team, even though some top players are using them - I reached level 8 in season 1 so don't consider me an expert! YMMV" ]
                    , p [] [ text "A summary page is available while battling - perhaps it will help you choose the right attack in the heat of the moment!" ]
                    , div [] [ mkStyledButton ( SwitchPage <| Registering [], "Start", True ) ]
                    ]

            Registering names ->
                div [ cls "choosing" ]
                    [ div [ class "my-pokemon flex flex-col flex-grow" ] (viewMyPokemons model league)
                    , div [ class "my-team flex flex-col flex-grow ml-2 mr-2" ] (viewTeam model league)
                    , div [ class "opponents flex flex-col flex-grow" ] (viewOpponentsRegistering model league names)
                    ]

            TeamOptions ->
                div [ cls "teams" ]
                    [ div [ class "my-pokemon flex flex-col flex-grow" ] (viewTeamOptions model league)
                    , div [ class "my-team flex flex-col flex-grow ml-2 mr-2" ] (viewTeam model league)
                    , div [ class "opponents flex flex-col flex-grow" ] (viewOpponentsBattling model league)
                    ]

            Battling ->
                div [ cls "battling" ]
                    [ div [ class "my-team flex flex-col flex-grow mr-2" ] (viewTeam model league)
                    , div [ class "opponents flex flex-col flex-grow" ] (viewOpponentsBattling model league)
                    ]

            FatalError string ->
                div [ cls "error" ]
                    [ h1 [] [ text "Fatal Error" ]
                    , div [] [ text string ]
                    ]
        , pvpFooter model.season
        ]


sortOpponents : Dict String Opponent -> List String
sortOpponents opponents =
    opponents
        |> Dict.toList
        |> L.sortBy (Tuple.second >> .frequency >> (*) -1)
        |> L.map Tuple.first


pvpHeader : List String -> Page -> Html Msg
pvpHeader lst tgt =
    header [ class "flex flex-row justify-between p-3 bg-gray-400" ]
        [ h1 [ class "text-2xl justify-center" ] [ text "Pokemon PVP team manager" ]
        , mkRadioButtons
            [ ( SwitchPage <| Registering lst, "Registering", isRegistering tgt )
            , ( SwitchPage TeamOptions, "Team options", TeamOptions == tgt )
            , ( SwitchPage Battling, "Battling", Battling == tgt )
            ]
        ]


pvpFooter : Season -> Html Msg
pvpFooter tgt =
    footer [ class "flex flex-row items-center justify-between p-3 bg-gray-400" ]
        [ span [ class "text-sm" ]
            [ text "Credits: Meta data from "
            , a
                [ href "https://pvpoke.com/"
                , class "underline"
                ]
                [ text "PvPoke" ]
            ]
        , mkRadioButtons
            [ ( SwitchSeason Great, "Great", Great == tgt )
            , ( SwitchSeason Ultra, "Ultra", Ultra == tgt )
            , ( SwitchSeason Master, "Master", Master == tgt )
            ]
        ]


mkRadioButtons : List ( msg, String, Bool ) -> Html msg
mkRadioButtons list =
    list
        |> L.map (mkStyledButton >> (\htm -> li [ class "mr-3" ] [ htm ]))
        |> ul [ class "flex" ]


mkStyledButton : ( msg, String, Bool ) -> Html msg
mkStyledButton ( msg, txt, selected ) =
    let
        cls b =
            if b then
                "inline-block border border-blue-500 rounded py-1 px-3 bg-blue-500 text-white"

            else
                "inline-block border border-white rounded hover:border-gray-200 text-blue-500 hover:bg-gray-200 py-1 px-3"
    in
    button
        [ class <| cls selected
        , onClick msg
        ]
        [ text txt ]



-- -------------------
-- LHS Choosing
-- -------------------


viewMyPokemons : Model -> League -> List (Html Msg)
viewMyPokemons model league =
    let
        chooser =
            case model.chooser of
                MyChooser search state ->
                    viewChooser model.pokedex search state

                _ ->
                    div [ class "flex flex-row items-center justify-between" ]
                        [ viewChooserPlaceholder <| MyChooser "" Autocomplete.empty
                        , pvpPokeLogo
                        ]

        rankings =
            if model.season == Ultra then
                model.rankings2500

            else
                Dict.empty

        addData idx pokemon =
            MyPokemonData idx
                pokemon
                (Dict.get pokemon.speciesId model.pokedex)
                (Dict.get pokemon.speciesId rankings)

        viewer : MyPokemonData -> Html Msg
        viewer { idx, pokemon, dex, mbRank } =
            Maybe.map (viewMyPokemon model idx pokemon mbRank) dex
                |> Maybe.withDefault
                    (div [ class <| cardClass ++ " mb-2 bg-red-200" ]
                        [ div [ class "flex flex-row items-center justify-between" ]
                            [ viewNameTitle pokemon.speciesId
                            , deleteIcon <| RemovePokemon idx
                            ]
                        , div [] [ text <| "Unexpected error looking up. Please delete and re-add" ]
                        ]
                    )
    in
    [ h2 [] [ text "My Pokemons" ]
    , chooser
    , if Array.isEmpty league.myPokemon then
        ul []
            [ ol [] [ text "Add you pokemon using the form above" ]
            , ol [] [ text "Select the attacks you are using" ]
            , ol [] [ text "Click on a title and then on one of the 'My Team' areas to add strt creating a team" ]
            ]

      else
        league.myPokemon
            |> Array.toList
            |> L.indexedMap addData
            |> L.sortBy (\{ mbRank } -> mbRank |> Maybe.map .score |> Maybe.withDefault 0 |> (*) -1)
            |> L.map viewer
            |> div []
    ]


type alias MyPokemonData =
    { idx : Int
    , pokemon : Pokemon
    , dex : Maybe PokedexEntry
    , mbRank : Maybe RankingEntry
    }


viewMyPokemon : Model -> Int -> Pokemon -> Maybe RankingEntry -> PokedexEntry -> Html Msg
viewMyPokemon model idx pokemon mbRanking entry =
    let
        mainCls =
            if Maybe.map .speciesId model.selectedPokemon == Just pokemon.speciesId then
                cardClass ++ " mb-2 bg-blue-100"

            else
                cardClass ++ " mb-2 bg-white"

        viewAttack_ selectMove isRec isSelected attack =
            let
                cls c =
                    class <| "flex flex-row items-center cursor-pointer rounded ml-1 p-1 " ++ c
            in
            span
                [ cls <| ifThenElse isSelected "bg-teal-300" "bg-gray-300"
                , onClick <| selectMove idx attack
                ]
            <|
                if isRec then
                    [ pvpPokeLogo, viewAttackBadge model.attacks attack ]

                else
                    [ viewAttackBadge model.attacks attack ]

        getRanks ranking =
            let
                ( p1, p2, p3 ) =
                    ranking.moveStr

                rFast =
                    LE.getAt p1 <| L.sort ranking.fastMoves

                rsCharged =
                    [ p2, p3 ] |> L.map (\p -> LE.getAt (p - 1) (L.sort ranking.chargedMoves))
            in
            ( rFast, rsCharged )

        ( recFast, recsCharged ) =
            mbRanking |> Maybe.map getRanks |> Maybe.withDefault ( Nothing, [] )

        attacksDetail =
            [ entry.fast
                |> L.map (\attack -> viewAttack_ SelectFastMove (Just attack == recFast) (attack == pokemon.fast) attack)
                |> (::) (text "Fast: ")
                |> div [ class "flex flex-row flex-wrap items-center ml-1 " ]
            , entry.charged
                |> L.map (\attack -> viewAttack_ SelectChargedMove (L.member (Just attack) recsCharged) (Set.member attack pokemon.charged) attack)
                |> (::) (text "Charged: ")
                |> div [ class "flex flex-row flex-wrap items-center" ]
            ]

        topLine =
            div [ class "flex flex-row items-center justify-between" ]
                [ div [ class "flex flex-row items-center" ]
                    [ toggleBtn (ToggleMyPokemon idx) pokemon.expanded
                    , h3
                        [ class "text-xl font-bold cursor-pointer truncate"
                        , onClick <| SelectCandidate pokemon
                        , title "Select for team"
                        ]
                        [ text <| entry.speciesName ]
                    , ppTypes entry.types
                    ]
                , div [ class "flex flex-row items-center text-sm" ]
                    [ if pokemon.expanded then
                        deleteIcon <| RemovePokemon idx

                      else
                        mbRanking |> Maybe.map (.score >> ppFloat) |> Maybe.withDefault "" |> text
                    ]

                -- getAttackTypes model.attacks entry
                ]
    in
    div [ class mainCls ] <|
        if pokemon.expanded then
            topLine :: attacksDetail

        else
            [ topLine ]



-- -------------------
-- LHS Teams
-- -------------------


viewTeamOptions : Model -> League -> List (Html Msg)
viewTeamOptions model league =
    let
        viewOption : ( ( String, String, String ), Float ) -> Html Msg
        viewOption ( ( c1, c2, c3 ), score ) =
            let
                selected =
                    hasMember c1 league.team && hasMember c2 league.team && hasMember c3 league.team

                title =
                    [ c1, c2, c3 ]
                        |> L.filterMap (\c -> Dict.get c model.pokedex |> Maybe.map .speciesName)
                        |> String.join ", "
            in
            div
                [ classList
                    [ ( cardClass, True )
                    , ( "mb-1 flex flex-row justify-between", True )
                    , ( "bg-blue-100", selected )
                    ]
                , onClick <| SetTeam ( c1, c2, c3 )
                ]
                [ span [] [ text title ]
                , span [ class "text-sm" ] [ text <| ppFloat score ]
                ]
    in
    [ h2 [] [ text "Team options" ]
    , Helpers.evaluateTeams league
        |> L.take 20
        |> L.map viewOption
        |> div [ class "flex flex-col" ]
    ]



-- -------------------
-- Middle: Team
-- -------------------


viewTeam : Model -> League -> List (Html Msg)
viewTeam model league =
    let
        team =
            league.team

        convertToPokedex : Pokemon -> PokedexEntry -> ( String, PokedexEntry )
        convertToPokedex pokemon pokedex =
            ( pokemon.speciesId, { pokedex | fast = [ pokemon.fast ], charged = Set.toList pokemon.charged } )

        lookupTeamMember : TeamMember -> Result String Pokemon
        lookupTeamMember teamMember =
            case teamMember of
                Unset ->
                    Err "Team member not chosen"

                Chosen speciesId ->
                    lookupName league.myPokemon speciesId

                Pinned speciesId ->
                    lookupName league.myPokemon speciesId

        viewMbCand updater mbCand =
            let
                handler name isPinned =
                    lookupName league.myPokemon name
                        |> Result.andThen
                            (\pokemon ->
                                model.pokedex
                                    |> Dict.get pokemon.speciesId
                                    |> Maybe.map (convertToPokedex pokemon)
                                    |> Result.fromMaybe ("Could not look up " ++ pokemon.speciesId ++ " in pokedex")
                            )
                        |> Result.map (\( _, entry ) -> viewTeamMember model name entry isPinned)
                        |> RE.extract (\err -> [ text err ])

                content =
                    case mbCand of
                        Unset ->
                            []

                        Chosen name ->
                            handler name False

                        Pinned name ->
                            handler name True
            in
            case model.selectedPokemon of
                Just selected ->
                    div
                        [ class "drop-zone p-1 mb-2 border-blue-500"
                        , onClick (updater selected.speciesId)
                        ]
                        content

                Nothing ->
                    div [ class "drop-zone p-1 mb-2 border-gray-500" ]
                        content

        sumFreqs =
            Helpers.calcWeightedTotal league.opponents

        score =
            Result.map3 (\a b c -> evaluateTeam ( a, b, c )) (lookupTeamMember team.cand1) (lookupTeamMember team.cand2) (lookupTeamMember team.cand3)
                |> Result.map (Helpers.summariseTeam league.opponents)
                |> Result.map (\x -> x / sumFreqs)
                |> Result.map (ppFloat >> (\s -> " (score: " ++ s ++ ")"))
                |> Result.withDefault ""
    in
    [ h2 [] [ text <| "My Team" ++ ifThenElse (model.page == Battling) "" score ]
    , viewMbCand (\c -> UpdateTeam { team | cand1 = Chosen c }) team.cand1
    , viewMbCand (\c -> UpdateTeam { team | cand2 = Chosen c }) team.cand2
    , viewMbCand (\c -> UpdateTeam { team | cand3 = Chosen c }) team.cand3
    ]


viewTeamMember : Model -> String -> PokedexEntry -> Bool -> List (Html Msg)
viewTeamMember model speciesId entry isPinned =
    let
        go attk acc =
            case attackToType model.attacks attk of
                Just tp ->
                    if L.member tp acc then
                        acc

                    else
                        tp :: acc

                Nothing ->
                    acc

        attacks =
            (entry.fast ++ entry.charged)
                |> L.foldl go []
                |> L.reverse

        viewAttk tp =
            div [ class "flex flex-row items-center mb-1" ]
                [ div [ class "mr-2" ] [ ppType tp ]
                , viewAttack1 effectiveness tp
                ]
    in
    [ div [ class "flex flex-row items-center justify-between mb-2" ]
        [ div [ class "flex flex-row items-center" ]
            [ viewNameTitle entry.speciesName
            , ppTypes entry.types
            ]
        , if model.page == TeamOptions then
            button [ onClick <| PinTeamMember speciesId ]
                [ matIcon <| ifThenElse isPinned "bookmark" "bookmark-outline" ]

          else
            text ""
        ]

    --, if model.page == Battling then
    --    attacks |> L.map viewAttk |> div []
    --
    --  else
    --    text ""
    ]
        ++ viewPokedexResistsAndWeaknesses entry



-- -------------------
-- RHS Opponents
-- -------------------


viewOpponentsRegistering : Model -> League -> List String -> List (Html Msg)
viewOpponentsRegistering model league names =
    let
        chooser =
            case model.chooser of
                OpponentChooser search state ->
                    viewChooser model.pokedex search state

                _ ->
                    div [ class "flex flex-row items-center justify-between" ]
                        [ viewChooserPlaceholder <| OpponentChooser "" Autocomplete.empty
                        , span [ class "text-sm" ] [ text "Frequency" ]
                        ]

        viewOpponent ( speciesId, op ) entry =
            let
                headerRow =
                    div [ class "flex flex-row align-items justify-between" ]
                        [ div
                            [ class "flex flex-row items-center" ]
                            [ toggleBtn (ToggleOpponent speciesId) op.expanded
                            , viewNameTitle entry.speciesName
                            , ppTypes entry.types
                            ]
                        , div [ class "flex flex-row items-center" ]
                            [ button [ onClick <| UpdateOpponentFrequency speciesId -1, class "ml-2 mr-1" ] [ text "-" ]
                            , span [ class "mr-1" ] [ text <| String.fromInt op.frequency ]
                            , button [ onClick <| UpdateOpponentFrequency speciesId 1, class "mr-1" ] [ text "+" ]
                            , if op.expanded then
                                deleteIcon <| RemoveOpponent speciesId

                              else
                                text ""
                            ]
                        ]

                content =
                    if op.expanded then
                        [ entry.fast
                            |> L.map (viewAttackBadge model.attacks)
                            |> (::) (span [ class "mr-2" ] [ text "Fast:" ])
                            |> div [ class "flex flex-row flex-wrap items-center ml-1 mb-2" ]
                        , entry.charged
                            |> L.map (viewAttackBadge model.attacks)
                            |> (::) (span [ class "mr-2" ] [ text "Charged:" ])
                            |> div [ class "flex flex-row flex-wrap items-center mb-2" ]
                        , div [] <| viewPokemonResistsAndWeaknesses model speciesId
                        ]

                    else
                        []
            in
            div [ class <| cardClass ++ " mb-2" ]
                (headerRow :: content)

        viewer : ( String, Opponent ) -> Html Msg
        viewer ( speciesId, op ) =
            case Dict.get speciesId model.pokedex of
                Just entry ->
                    viewOpponent ( speciesId, op ) entry

                Nothing ->
                    div [ class "flex flex-row justify-between" ]
                        [ text <| "Could not look up " ++ speciesId
                        , deleteIcon <| RemoveOpponent speciesId
                        ]
    in
    [ h2 [] [ text "Opponents" ]
    , chooser
    , names
        |> L.filterMap (\n -> Dict.get n league.opponents |> Maybe.map (Tuple.pair n))
        |> L.map viewer
        |> div []
    ]


viewOpponentsBattling : Model -> League -> List (Html Msg)
viewOpponentsBattling model league =
    let
        team : List ( String, PType )
        team =
            summariseTeam model league

        mkBadge ( mySpeciesId, pType ) =
            model.pokedex
                |> Dict.get mySpeciesId
                |> Maybe.map .speciesName
                |> Maybe.withDefault mySpeciesId
                |> colouredBadge pType

        viewOpponent speciesId entry =
            let
                ( weak, resists ) =
                    checkAttackAgainstDefenderType effectiveness team entry.types

                viewLst cls lst =
                    if L.isEmpty lst then
                        text ""

                    else
                        div [ class <| "flex flex-row flex-wrap items-baseline ml-4 p-1 " ++ cls ] <|
                            L.map mkBadge lst
            in
            div [ class <| cardClass ++ " flex flex-row  items-center justify-between mb-1" ]
                [ div [ class "flex flex-row items-center" ]
                    [ div []
                        [ viewNameTitle entry.speciesName
                        , if model.debug then
                            small [ class "text-xs" ] [ text <| calcTeamScores model league speciesId ]

                          else
                            text ""
                        ]
                    , viewLst "bg-green-200" weak
                    ]
                , viewLst "bg-red-200" resists
                ]

        viewer ( speciesId, _ ) =
            case Dict.get speciesId model.pokedex of
                Just entry ->
                    viewOpponent speciesId entry

                Nothing ->
                    text <| "Could not look up " ++ speciesId
    in
    [ h2 [] [ text "Opponents" ]
    , league.opponents
        |> Dict.toList
        |> L.sortBy (Tuple.second >> .frequency >> (*) -1)
        |> L.map viewer
        |> div [ class "flex flex-col" ]
    ]


checkAttackAgainstDefenderType : Effectiveness -> List ( String, PType ) -> List PType -> ( List ( String, PType ), List ( String, PType ) )
checkAttackAgainstDefenderType effectiveness team defenderTypes =
    let
        go : ( String, PType ) -> ( List ( String, PType ), List ( String, PType ) ) -> ( List ( String, PType ), List ( String, PType ) )
        go (( _, attackTp ) as val) ( accWeak, accResists ) =
            let
                score =
                    Dict.get attackTp effectiveness
                        |> Maybe.map (\matrix -> calculateEffectiveness defenderTypes matrix)
                        |> Maybe.withDefault 0
            in
            if score > 1.1 then
                ( val :: accWeak, accResists )

            else if score < 0.9 then
                ( accWeak, val :: accResists )

            else
                ( accWeak, accResists )
    in
    L.foldr go ( [], [] ) team


summariseTeam : Model -> League -> List ( String, PType )
summariseTeam model league =
    [ league.team.cand1
    , league.team.cand2
    , league.team.cand3
    ]
        |> L.filterMap getMember
        |> summariseTeam2 model.attacks league.myPokemon


calcTeamScores : Model -> League -> String -> String
calcTeamScores model league opName =
    let
        mkItemInner pokemon =
            Helpers.evaluateBattle model.pokedex model.attacks pokemon opName
                |> Result.toMaybe

        mkItem : String -> String
        mkItem cand =
            Maybe.map mkItemInner
                (lookupMyPokemon league.myPokemon cand)
                |> Maybe.andThen identity
                |> Maybe.map ppFloat
                |> Maybe.withDefault "."
    in
    [ league.team.cand1
    , league.team.cand2
    , league.team.cand3
    ]
        |> L.filterMap (getMember >> Maybe.map mkItem)
        |> String.join ", "


summariseTeam2 : Dict String MoveType -> Array Pokemon -> List String -> List ( String, PType )
summariseTeam2 attacks myPokemon team =
    team
        |> L.filterMap (lookupMyPokemon myPokemon)
        |> summariseTeamInner attacks


lookupMyPokemon : Array Pokemon -> String -> Maybe Pokemon
lookupMyPokemon myPokemon name =
    myPokemon
        |> Array.filter (\item -> item.speciesId == name)
        |> Array.get 0


summariseTeamInner : Dict String MoveType -> List Pokemon -> List ( String, PType )
summariseTeamInner attacks pokemons =
    let
        createItem : String -> ( String, String ) -> Maybe ( String, PType )
        createItem name ( attack, _ ) =
            Dict.get attack attacks
                |> Maybe.map (\{ type_ } -> ( name, type_ ))

        go : Pokemon -> List ( String, PType ) -> List ( String, PType )
        go p acc =
            p.charged
                |> Set.toList
                |> L.map (\atk -> ( atk, "**" ))
                |> (::) ( p.fast, "*" )
                |> L.filterMap (createItem p.speciesId)
                |> (++) acc
    in
    L.foldl go [] pokemons



-- -------------------
-- View pokemon
-- -------------------


ppFloat =
    FormatNumber.format { usLocale | decimals = Exact 1 }


viewNameTitle name =
    h3 [ class "text-xl font-bold truncate" ] [ text name ]


viewPokemonResistsAndWeaknesses : Model -> String -> List (Html msg)
viewPokemonResistsAndWeaknesses model name =
    model.pokedex
        |> Dict.get name
        |> Maybe.map viewPokedexResistsAndWeaknesses
        |> Maybe.withDefault []


viewPokedexResistsAndWeaknesses : PokedexEntry -> List (Html msg)
viewPokedexResistsAndWeaknesses entry =
    let
        effectivenesses =
            getDefenceMeta effectiveness entry.types
    in
    [ viewTypes (\_ f -> f > 1.1) effectivenesses "Weak to"
    , viewTypes (\_ f -> f < 0.9) effectivenesses "Resists"
    ]


getDefenceMeta : Effectiveness -> List PType -> Dict PType Float
getDefenceMeta effectiveness tps =
    let
        go _ dict =
            L.foldl (\tp acc -> Dict.get tp dict |> Maybe.withDefault 1 |> (*) acc) 1 tps
    in
    effectiveness
        |> Dict.map go


viewTypes : (PType -> Float -> Bool) -> Dict PType Float -> String -> Html msg
viewTypes fn weaknesses title =
    let
        ( normals, supers ) =
            weaknesses
                |> Dict.filter fn
                |> Dict.toList
                |> L.partition (\( _, v ) -> v > 0.5 && v < 1.5)
    in
    div [ class "flex flex-row items-center mb-2" ]
        [ span [ class "mr-3" ] [ text title ]
        , div [ class "badge-list flex flex-row items-center" ]
            [ supers |> L.map (\( tp, _ ) -> span [ class "super mr-3" ] [ ppType tp ]) |> div [ class "flex flex-row" ]
            , normals |> L.map (\( tp, _ ) -> span [ class "mr-1" ] [ ppType tp ]) |> div [ class "flex flex-row flex-wrap" ]
            ]
        ]


getAttackTypes : Dict String MoveType -> PokedexEntry -> Html msg
getAttackTypes attacks entry =
    let
        fn : String -> List PType -> List PType
        fn attk acc =
            case attackToType attacks attk of
                Just tp ->
                    if L.member tp acc then
                        acc

                    else
                        tp :: acc

                Nothing ->
                    acc
    in
    (entry.fast ++ entry.charged)
        |> L.foldl fn []
        |> L.map ppTypeShort
        |> div [ class "flex flex-row text-xs" ]


attackToType : Dict String MoveType -> String -> Maybe PType
attackToType attacks attack =
    if String.startsWith "HIDDEN" attack then
        Nothing

    else
        attacks
            |> Dict.get attack
            |> Maybe.map .type_


viewAttack1 : Dict PType (Dict PType Float) -> PType -> Html msg
viewAttack1 effectiveness attack =
    let
        go tp val ( accStrong, accWeak ) =
            if val < 0.9 then
                ( accStrong, tp :: accWeak )

            else if val > 1.1 then
                ( tp :: accStrong, accWeak )

            else
                ( accStrong, accWeak )

        ( strong, weak ) =
            effectiveness
                |> Dict.get attack
                |> Maybe.map (Dict.foldl go ( [], [] ))
                |> Maybe.withDefault ( [], [] )
    in
    div [ class "flex flex-col" ]
        [ strong |> L.map ppType |> div []
        , weak |> L.map ppType |> div []
        ]


viewAttackBadge : Dict String MoveType -> String -> Html msg
viewAttackBadge attacks attack =
    case Dict.get attack attacks of
        Just mt ->
            badge (Tuple.second <| stringFromPType mt.type_) mt.name

        Nothing ->
            text attack



-- -------------------
-- Chooser
-- -------------------


viewChooser : Dict String PokedexEntry -> String -> Autocomplete.State -> Html Msg
viewChooser pokedex search autocomplete =
    let
        choices =
            getRelevantChoices search pokedex
    in
    div [ class "chooser-container flex flex-row justify-between mb-2" ]
        [ Autocomplete.view viewConfig autocomplete choices search
            |> Html.map ACMsg
        , span [ onClick <| SetAutoComplete NoChooser ] [ matIcon "close" ]
        ]


viewChooserPlaceholder : PokedexChooser -> Html Msg
viewChooserPlaceholder chooser =
    div
        [ class "flex flex-row mb-2 cursor-pointer"
        , onClick <| SetAutoComplete chooser
        ]
        [ span [ class "mr-2" ] [ matIcon "pencil" ]
        , text "Add pokemon"
        ]


viewConfig : Autocomplete.ViewConfig ( String, PokedexEntry )
viewConfig =
    let
        customizedLi keySelected mouseSelected ( _, item ) =
            { attributes =
                [ classList
                    [ ( "autocomplete-item", True )
                    , ( "key-selected", keySelected )
                    , ( "mouse-selected", mouseSelected )
                    ]
                ]
            , children = [ Html.text item.speciesName ]
            }
    in
    { toId = toId
    , ul = []
    , li = customizedLi
    }



-- -------------------
-- View Helpers
-- -------------------


toggleBtn msg expanded =
    button
        [ class "toggle"
        , onClick msg
        ]
        [ matIcon <| ifThenElse expanded "chevron-down" "chevron-right" ]


cardClass =
    "rounded overflow-hidden shadow-lg p-1 bg-white"


pvpPokeLogo =
    img
        [ src "images/pvpoke.png"
        , style "height" "20px"
        , class "mr-1"
        ]
        []


deleteIcon : Msg -> Html Msg
deleteIcon msg =
    span
        [ class "btn-delete ml-1 cursor-pointer"
        , onClick msg
        ]
        [ matIcon "delete-outline" ]


ppTypes : List PType -> Html msg
ppTypes types =
    types |> L.map (\tp -> span [ class "ml-2" ] [ ppType tp ]) |> div [ class "flex flex-row items-center" ]


ppType : PType -> Html msg
ppType pType =
    let
        ( str, col ) =
            stringFromPType pType
    in
    badge col str


ppTypeShort : PType -> Html msg
ppTypeShort pType =
    let
        ( str, col ) =
            stringFromPType pType
    in
    div
        [ style "background-color" col
        , class "flex flex-row items-center justify-center badge round small"
        ]
        [ text <| String.left 1 str ]


colouredBadge : PType -> String -> Html msg
colouredBadge pType str =
    let
        ( _, col ) =
            stringFromPType pType
    in
    badge col str


badge : String -> String -> Html msg
badge col str =
    span
        [ style "background-color" col
        , class "badge truncate p-1 rounded text-sm"
        ]
        [ text str ]


matIcon t =
    span [ class <| "mdi mdi-" ++ t ] []



-- Main program


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "Autocomplete example"
                , body = [ view m ]
                }
        , subscriptions = \_ -> Sub.none
        }



--
--viewAttacks model entry =
--    div [ class "flex flex-col" ]
--        [ entry.fast
--            |> L.map (viewAttack model)
--            |> div [ class "flex flex-col" ]
--        , entry.charged
--            |> L.map (viewAttack model)
--            |> div [ class "flex flex-col" ]
--        ]
--viewAttack : Model -> String -> Html msg
--viewAttack model attack =
--    div [ class "flex flex-row items-center" ]
--        [ div [ class "mr-2" ] [ viewAttackBadge model.attacks attack ]
--        , model.attacks
--            |> Dict.get attack
--            |> Maybe.map (.type_ >> viewAttack1 effectiveness)
--            |> Maybe.withDefault (text attack)
--        ]
--calcTeamEffectiveness : Effectiveness -> List PokedexEntry -> List ( String, PType ) -> Float
--calcTeamEffectiveness effectiveness opponents teamAttacks =
--let
--    ctOps =
--        L.length opponents |> toFloat
--
--    ctAttacks =
--        L.length teamAttacks |> toFloat
--
--    scoreAttack : ( String, PType ) -> Float
--    scoreAttack ( _, attackTp ) =
--        L.foldl (\op acc -> acc + calculateEffectiveness effectiveness attackTp op.types) 0 opponents
--in
--L.foldl (\attack acc -> acc + scoreAttack attack) 0 teamAttacks / ctOps / ctAttacks
--
--addScores : Model -> League -> League
--addScores model league =
--    let
--        opponents =
--            league.opponents
--                |> L.filterMap (\opName -> Dict.get opName model.pokedex)
--
--        scorer =
--            calcPokemonScore model.attacks effectiveness opponents
--    in
--    { league | myPokemon = Array.map scorer league.myPokemon }
--calcPokemonScore : Dict String MoveType -> Effectiveness -> List PokedexEntry -> Pokemon -> Pokemon
--calcPokemonScore attacks effectiveness opponents pokemon =
--    let
--        thisAttacks : List ( String, PType )
--        thisAttacks =
--            (pokemon.fast :: Set.toList pokemon.charged)
--                |> L.filterMap (nameToAttack attacks)
--    in
--    { pokemon | scores = calcTeamEffectiveness effectiveness opponents thisAttacks }
--nameToAttack : Dict String MoveType -> String -> Maybe ( String, PType )
--nameToAttack moves attack =
--    moves
--        |> Dict.get attack
--        |> Maybe.map (.type_ >> Tuple.pair attack)
