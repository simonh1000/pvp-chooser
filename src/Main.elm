module Main exposing (main, viewAttacksWithRecommendations)

import AssocList as AL
import Autocomplete exposing (..)
import Browser
import Common.CoreHelpers exposing (addCmd, ifThenElse, rejectByList)
import Dict exposing (Dict)
import FormatNumber
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import Helpers exposing (addScoresToLeague, calculateEffectiveness, evaluateTeam, lookupName)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Extra exposing (andMap)
import List as L
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
                        , debug = flags.debug
                    }

                page =
                    case flags.persisted.season of
                        Just _ ->
                            LoadingDex

                        Nothing ->
                            Intro
            in
            ( addScores { model | page = page }, getPokedex )

        Err err ->
            ( { defaultModel | page = FatalError <| Decode.errorToString err }, Cmd.none )


type alias Flags =
    { persisted : Persisted
    , debug : Bool
    }


decodeFlags : Decoder Flags
decodeFlags =
    Decode.succeed Flags
        |> andMap (Decode.field "myPokemon" decodePersisted)
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
    | SetAutoComplete SearchTool
      -- Registering: My pokemons
    | ToggleMyPokemon String
    | SelectFastMove String String
    | SelectChargedMove String String
    | RemovePokemon String
    | SelectCandidate String -- speciesId
      -- Registering: Team
    | UpdateTeam Team -- second part
    | SwapTeam Bool -- isTopTwo
      -- Team chooser
    | PinTeamMember String
      -- Registering: opponents
    | ToggleOpponent String
    | UpdateOpponentFrequency String Int -- name
    | RemoveOpponent String
      --
    | OnPokedex (Result Http.Error ( Dict String MoveType, Pokedex ))
    | OnRankingData Season (Result Http.Error (Dict String RankingEntry))


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        SwitchPage page ->
            ( { model | page = page }, Cmd.none )

        SwitchSeason season ->
            if season == model.season then
                ( model, Cmd.none )

            else
                let
                    newModel =
                        { model
                            | season = season
                            , pokedex = resetPokedex model.pokedex
                        }

                    newPage =
                        case model.page of
                            Registering m ->
                                { m | opponents = newModel |> getCurrentLeague |> .opponents |> sortOpponents } |> Registering

                            p ->
                                p
                in
                { newModel | page = newPage }
                    |> andPersist
                    |> addCmd (getRankings season)

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
                                    | fast = model.pokedex |> Dict.get speciesId |> Maybe.andThen (\{ fast } -> L.head fast) |> Maybe.withDefault ""
                                    , charged = Set.empty
                                }
                        in
                        { l | myPokemon = Dict.insert speciesId pokemon l.myPokemon }

                    else
                        { l | opponents = Dict.insert speciesId (Opponent False 1) l.opponents }

                newModel =
                    updateLeague updater model

                page =
                    case model.page of
                        Registering m ->
                            if isMyPokemon then
                                Registering m

                            else
                                Registering <| { m | opponents = speciesId :: m.opponents }

                        p ->
                            p
            in
            { newModel
                | chooser = resetSearch model.chooser
                , page = page
            }
                |> andPersist

        SetAutoComplete chooser ->
            ( { model | chooser = chooser }, Cmd.none )

        -- My Pokemons
        ToggleMyPokemon speciesId ->
            model
                |> updateLeague (\l -> { l | myPokemon = Dict.update speciesId (Maybe.map <| \p -> { p | expanded = not p.expanded }) l.myPokemon })
                |> andPersist

        SelectFastMove speciesId move ->
            model
                |> updateLeague (\l -> { l | myPokemon = Dict.update speciesId (Maybe.map <| \p -> { p | fast = move }) l.myPokemon })
                |> andPersist

        SelectChargedMove speciesId move ->
            model
                |> updateLeague (\l -> { l | myPokemon = Dict.update speciesId (Maybe.map <| toggleCharged move) l.myPokemon })
                |> andPersist

        RemovePokemon speciesId ->
            let
                updater l =
                    { l
                        | myPokemon = Dict.remove speciesId l.myPokemon
                        , team = removeFromTeam speciesId l.team
                    }
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

        UpdateTeam team ->
            -- mapRegistering has no effect on TeamChoosing page
            { model | page = mapRegistering (\m -> { m | selectedPokemon = Nothing }) model.page }
                |> updateLeague (\l -> { l | team = team })
                |> andPersist

        -- team chooser
        SelectCandidate pokemon ->
            let
                mapFn m =
                    { m | selectedPokemon = ifThenElse (Just pokemon == m.selectedPokemon) Nothing (Just pokemon) }
            in
            ( { model | page = mapRegistering mapFn model.page }, Cmd.none )

        SwapTeam isTopTwo ->
            let
                updater l =
                    { l
                        | team =
                            if isTopTwo then
                                { cand1 = l.team.cand2, cand2 = l.team.cand1, cand3 = l.team.cand3 }

                            else
                                { cand1 = l.team.cand1, cand2 = l.team.cand3, cand3 = l.team.cand2 }
                    }
            in
            model
                |> updateLeague updater
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

        OnPokedex res ->
            case res of
                Ok ( moves, pokedex ) ->
                    ( addScores
                        { model
                            | page = mkRegisteringPage model
                            , moves = moves
                            , pokedex = pokedex
                        }
                    , getRankings model.season
                    )

                Err _ ->
                    ( { model | page = FatalError "Could not load gamemaster data" }, Cmd.none )

        OnRankingData season res ->
            case res of
                Ok rankings ->
                    if season == model.season then
                        ( addScores
                            { model
                                | pokedex = attachRankings rankings model.pokedex
                                , errorMessage = Nothing
                            }
                        , Cmd.none
                        )

                    else
                        -- user has changed season - we do not want this data any more
                        ( { model | errorMessage = Nothing }, Cmd.none )

                Err _ ->
                    ( { model | errorMessage = Just "Could not access ranking data" }
                    , Cmd.none
                    )


mkRegisteringPage : Model -> Page
mkRegisteringPage model =
    if model.page == Intro then
        Intro

    else
        let
            opponents =
                model |> getCurrentLeague |> .opponents |> sortOpponents
        in
        Registering { blankRegistering | opponents = opponents }


andPersist : Model -> ( Model, Cmd msg )
andPersist model =
    ( addScores model
    , Ports.persist <| encodePersisted model
    )


addScores : Model -> Model
addScores model =
    { model
        | great = addScoresToLeague model model.great
        , ultra = addScoresToLeague model model.ultra
        , master = addScoresToLeague model model.master
    }


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
            getCurrentLeague model

        lst =
            sortOpponents league.opponents
    in
    div [ class "h-screen flex flex-col" ]
        [ pvpHeader lst model.page
        , case model.page of
            Intro ->
                div [ class "intro flex-grow p-3" ]
                    [ h2 [] [ text "Introduction" ]
                    , p [] [ text "This app will help you keep track of your pokemon and their types as well as of the competitors you encounter. The data you add is stored on your computer and nowhere else." ]
                    , img [ src "images/screenshot.png", class "mb-2" ] []
                    , p [] [ text "To start, you add your pokemon for each league, and those that you encounter in combat. You select you pokemons' attacks, and the PvPoke recommendations are shown in line (Ultra League only at present)." ]
                    , p [] [ text "The app enables you to build teams of three and to compare them. Each team gets a score. The absolute value is meaningless, but the relative scores may help you. The algorithm focuses on type dominance and does not take into account the details of energy generation and usage. Consequently, it is unlikely to recommend an unbalanced team, even though some top players are using them - I reached level 8 in season 1 so don't consider me an expert! YMMV" ]
                    , p [] [ text "A summary page is available while battling - perhaps it will help you choose the right attack in the heat of the moment!" ]
                    , div [] [ mkStyledButton ( SwitchPage <| Registering blankRegistering, "Start", True ) ]
                    ]

            LoadingDex ->
                div [ class "loading flex-grow" ] []

            Registering m ->
                div [ cls "choosing grid grid-cols-1 md:grid-cols-3 gap-2" ]
                    [ div [ class "my-pokemon flex flex-col" ] (viewMyPokemons model m league)
                    , div [ class "my-team flex flex-col" ] (viewTeam model m.selectedPokemon league)
                    , div [ class "opponents flex flex-col" ] (viewOpponentsRegistering model league m.opponents)
                    ]

            TeamOptions ->
                div [ cls "teams grid grid-cols-1 md:grid-cols-4 gap-2" ]
                    [ div [ class "my-pokemon flex flex-col" ] (viewTeamOptions model league)
                    , div [ class "my-team flex flex-col" ] (viewTeam model Nothing league)
                    , div [ class "opponents flex flex-col col-span-2" ] (viewOpponentsBattling model league)
                    ]

            Battling ->
                div [ cls "battling grid grid-cols-1 md:grid-cols-3 gap-2" ]
                    [ div [ class "my-team flex flex-col" ] (viewTeam model Nothing league)
                    , div [ class "opponents flex flex-col col-span-2" ] (viewOpponentsBattling model league)
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
    let
        switcher =
            mkRadioButtons
                [ ( SwitchPage <| Registering { blankRegistering | opponents = lst }, "Registering", isRegistering tgt )
                , ( SwitchPage TeamOptions, "Team options", TeamOptions == tgt )
                , ( SwitchPage Battling, "Battling", Battling == tgt )
                ]
    in
    header [ class "flex flex-row justify-between p-3 bg-gray-400" ]
        [ h1 [ class "text-2xl justify-center" ] [ text "Pokemon PVP team manager" ]
        , case tgt of
            Intro ->
                text ""

            FatalError _ ->
                text ""

            _ ->
                switcher
        ]


pvpFooter : Season -> Html Msg
pvpFooter tgt =
    footer [ class "flex flex-row items-center justify-between p-3 bg-gray-400" ]
        [ span [ class "text-sm" ]
            [ span [] [ text "Credits: Meta data from ", a [ href "https://pvpoke.com/" ] [ text "PvPoke" ] ]
            , span [] [ text ", Privacy: Uses Google Analytics" ]
            , span [] [ text ", Code: ", a [ href "https://github.com/simonh1000/pvp-chooser" ] [ text "Github" ] ]
            , span [] [ text ", Feedback: ", a [ href "https://twitter.com/lambda_simon" ] [ text "@lambda_simon" ] ]
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
        cls =
            if selected then
                "inline-block border border-blue-500 rounded py-1 px-3 bg-blue-500 text-white"

            else
                "inline-block border border-white rounded hover:border-gray-200 text-blue-500 hover:bg-gray-200 py-1 px-3"
    in
    button
        [ class cls
        , onClick msg
        ]
        [ text txt ]



-- -------------------
-- LHS Choosing
-- -------------------


viewMyPokemons : Model -> RegisteringModel -> League -> List (Html Msg)
viewMyPokemons model m league =
    let
        chooser =
            div [ class "flex flex-row items-center justify-between mb-2" ] <|
                case model.chooser of
                    MyChooser search state ->
                        [ viewChooser model.pokedex search state ]

                    _ ->
                        [ viewChooserPlaceholder <| MyChooser "" Autocomplete.empty
                        , pvpPokeLogo
                        ]

        addData speciesId pokemon =
            MyPokemonData speciesId pokemon (Dict.get speciesId model.pokedex)

        viewer : MyPokemonData -> Html Msg
        viewer { speciesId, pokemon, dex } =
            Maybe.map (viewMyPokemon model m.selectedPokemon speciesId pokemon) dex
                |> Maybe.withDefault
                    (div [ class <| cardClass ++ " mb-2 bg-red-200" ]
                        [ div [ class "flex flex-row items-center justify-between" ]
                            [ viewNameTitle speciesId
                            , deleteIcon <| RemovePokemon speciesId
                            ]
                        , div [] [ text <| "Unexpected error looking up. Please delete and re-add" ]
                        ]
                    )
    in
    [ h2 [] [ text "My Pokemons" ]
    , chooser
    , if Dict.isEmpty league.myPokemon then
        ul []
            [ ol [ class "mb-3" ] [ matIcon "arrow-up-bold", matIcon "arrow-up-bold", text "First add some of your pokemon using the form above" ]
            , ol [ class "mb-3" ] [ text "Select the attacks you are using" ]
            , ol [ class "mb-3" ] [ text "Click on a Pokemon's name and then on one of the 'My Team' drop-zones to add to your team" ]
            ]

      else
        league.myPokemon
            |> rejectByList (L.filterMap extractSpeciesId <| mkTeamList league.team)
            |> Dict.map addData
            |> Dict.values
            |> L.sortBy (\{ dex } -> dex |> Maybe.andThen .score |> Maybe.withDefault 0 |> (*) -1)
            |> L.map viewer
            |> div []
    ]


type alias MyPokemonData =
    { speciesId : String
    , pokemon : Pokemon
    , dex : Maybe PokedexEntry
    }


viewMyPokemon : Model -> Maybe String -> String -> Pokemon -> PokedexEntry -> Html Msg
viewMyPokemon model selectedPokemon speciesId pokemon entry =
    let
        mainCls =
            if selectedPokemon == Just speciesId then
                " mb-2 bg-blue-100"

            else
                " mb-2 bg-white"

        topLine =
            div [ class "flex flex-row items-center justify-between" ]
                [ -- LHS
                  div [ class "flex flex-row items-center" ]
                    [ toggleBtn (ToggleMyPokemon speciesId) pokemon.expanded
                    , ppTypes entry.types
                    , div
                        [ onClick <| SelectCandidate speciesId
                        , title "Select for team"
                        , class "cursor-pointer"
                        ]
                        [ viewNameTitle entry.speciesName ]
                    ]
                , -- RHS
                  div [ class "flex flex-row items-center text-sm" ]
                    [ ifThenElse pokemon.expanded (text "") (summariseMoves model.moves pokemon)
                    , if pokemon.expanded then
                        deleteIcon <| RemovePokemon speciesId

                      else
                        entry.score |> Maybe.map ppFloat |> Maybe.withDefault "" |> text
                    ]

                -- getAttackTypes model.attacks entry
                ]
    in
    div [ class <| cardClass ++ mainCls ] <|
        if pokemon.expanded then
            topLine :: viewAttacksWithRecommendations model.moves entry speciesId pokemon

        else
            [ topLine ]


viewAttacksWithRecommendations : Dict String MoveType -> PokedexEntry -> String -> Pokemon -> List (Html Msg)
viewAttacksWithRecommendations moves entry speciesId pokemon =
    let
        viewAttack_ selectMove isSelected attack =
            span
                [ class <| "flex flex-row items-center cursor-pointer rounded ml-1 p-1 " ++ ifThenElse isSelected "bg-teal-300" "bg-transparent"
                , onClick <| selectMove speciesId attack
                ]
            <|
                [ viewMoveWithPvPoke moves entry attack ]
    in
    [ entry.fast
        |> L.map (\attack -> viewAttack_ SelectFastMove (attack == pokemon.fast) attack)
        |> (::) (text "Fast: ")
        |> div [ class "flex flex-row flex-wrap items-center ml-1 " ]
    , entry.charged
        |> L.map (\attack -> viewAttack_ SelectChargedMove (Set.member attack pokemon.charged) attack)
        |> (::) (text "Charged: ")
        |> div [ class "flex flex-row flex-wrap items-center" ]
    , if Set.size pokemon.charged > 2 then
        div [ class "text-red-400" ] [ text "You have selected more than 2 charged moves" ]

      else
        text ""
    ]


summariseMoves : Dict String MoveType -> Pokemon -> Html msg
summariseMoves attacks pokemon =
    let
        convert attk =
            case attackToType attacks attk of
                Just tp ->
                    ppTypeShort tp

                Nothing ->
                    text "?"
    in
    div [ class "flex flex-row border border-gray-400 rounded-sm divide-x divide-gray-400 mr-2" ]
        [ span [ class "p-1" ] [ convert pokemon.fast ]
        , pokemon.charged |> Set.toList |> L.map convert |> span [ class "flex flex-row p-1" ]
        ]



-- -------------------
-- LHS Teams
-- -------------------


viewTeamOptions : Model -> League -> List (Html Msg)
viewTeamOptions model league =
    let
        viewOption : ( Team, Float ) -> Html Msg
        viewOption ( { cand1, cand2, cand3 } as team, score ) =
            let
                selected =
                    team == league.team

                title =
                    [ cand1, cand2, cand3 ]
                        |> L.filterMap (\c -> c |> extractSpeciesId |> Maybe.andThen (\id -> Dict.get id model.pokedex) |> Maybe.map .speciesName)
                        |> String.join ", "
            in
            div
                [ classList
                    [ ( cardClass, True )
                    , ( "mb-1 flex flex-row justify-between cursor-pointer", True )
                    , ( "bg-blue-100", selected )
                    ]
                , onClick <| UpdateTeam team
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


viewTeam : Model -> Maybe String -> League -> List (Html Msg)
viewTeam model selectedPokemon league =
    let
        team =
            league.team

        lookupTeamMember : TeamMember -> Result String Pokemon
        lookupTeamMember =
            extractSpeciesId >> Result.fromMaybe "Team member not chosen" >> Result.andThen (lookupName league.myPokemon)

        viewMbCand updater mbCand =
            let
                handler name isPinned =
                    lookupName league.myPokemon name
                        |> Result.andThen
                            (\pokemon ->
                                model.pokedex
                                    |> Dict.get name
                                    |> Maybe.map (Tuple.pair pokemon)
                                    |> Result.fromMaybe ("Could not look up " ++ name ++ " in pokedex")
                            )
                        |> Result.map (\( pokemon, entry ) -> viewTeamMember updater model name entry isPinned pokemon)
                        |> RE.extract (\err -> [ text err ])

                content =
                    case mbCand of
                        Unset ->
                            []

                        Chosen name ->
                            handler name False

                        Pinned name ->
                            handler name True

                overlay =
                    div [ class "overlay flex flex-row items-center justify-center" ]
                        [ text "Click here to insert into team" ]
            in
            case selectedPokemon of
                Just selected ->
                    div
                        [ class "drop-zone p-1 mb-2 border-blue-500 relative"
                        , onClick (updater <| Chosen selected)
                        ]
                        (overlay :: content)

                Nothing ->
                    div [ class "drop-zone p-1 mb-2 border-gray-500" ]
                        content

        sumFreqs =
            Helpers.calcWeightedTotal league.opponents

        score =
            Result.map3 (\a b c -> evaluateTeam ( a, b, c )) (lookupTeamMember team.cand1) (lookupTeamMember team.cand2) (lookupTeamMember team.cand3)
                |> Result.map (Helpers.summariseTeam league.opponents)
                |> Result.map (\x -> x / sumFreqs)
                |> Result.map (ppFloat >> (\s -> "Score: " ++ s))
                |> Result.withDefault ""
    in
    [ h2 [] [ text <| "My Team" ]
    , if isRegistering model.page then
        div [ class "spacer" ] [ text score ]

      else
        text ""
    , viewMbCand (\c -> UpdateTeam { team | cand1 = c }) team.cand1
    , div [ class "flex flex-row justify-center mb-2" ] [ button [ onClick <| SwapTeam True ] [ matIcon "swap-vertical-bold" ] ]
    , viewMbCand (\c -> UpdateTeam { team | cand2 = c }) team.cand2
    , div [ class "flex flex-row justify-center mb-2" ] [ button [ onClick <| SwapTeam False ] [ matIcon "swap-vertical-bold" ] ]
    , viewMbCand (\c -> UpdateTeam { team | cand3 = c }) team.cand3
    ]


viewTeamMember : (TeamMember -> Msg) -> Model -> String -> PokedexEntry -> Bool -> Pokemon -> List (Html Msg)
viewTeamMember updater model speciesId entry isPinned pokemon =
    let
        topLine =
            div [ class "flex flex-row items-center justify-between mb-2" ]
                [ div [ class "flex flex-row items-center" ]
                    [ ppTypes entry.types
                    , viewNameTitle entry.speciesName
                    , span [ class "ml-2" ] [ summariseMoves model.moves pokemon ]
                    ]
                , case model.page of
                    TeamOptions ->
                        button [ onClick <| PinTeamMember speciesId ]
                            [ matIcon <| ifThenElse isPinned "bookmark" "bookmark-outline" ]

                    Registering _ ->
                        div [ class "flex flex-row" ]
                            [ span [ class "mr-2 text-sm" ] [ entry.score |> Maybe.map ppFloat |> Maybe.withDefault "" |> text ]
                            , button [ onClick <| updater Unset ] [ matIcon "bookmark-remove" ]
                            ]

                    _ ->
                        text ""
                ]

        middlePart =
            case model.page of
                Registering _ ->
                    viewAttacksWithRecommendations model.moves entry speciesId pokemon

                _ ->
                    []
    in
    topLine :: middlePart ++ viewPokedexResistsAndWeaknesses entry



-- -------------------
-- RHS Opponents
-- -------------------


viewOpponentsRegistering : Model -> League -> List String -> List (Html Msg)
viewOpponentsRegistering model league names =
    let
        chooser =
            div [ class "flex flex-row items-center justify-between mb-2 " ] <|
                case model.chooser of
                    OpponentChooser search state ->
                        [ viewChooser model.pokedex search state ]

                    _ ->
                        [ viewChooserPlaceholder <| OpponentChooser "" Autocomplete.empty
                        , span [ class "text-sm" ] [ text "Frequency" ]
                        ]

        viewOpponent : ( String, Opponent ) -> PokedexEntry -> Html Msg
        viewOpponent ( speciesId, op ) entry =
            let
                headerRow =
                    div [ class "flex flex-row align-items justify-between" ]
                        [ div
                            [ class "flex flex-row items-center" ]
                            [ toggleBtn (ToggleOpponent speciesId) op.expanded
                            , ppTypes entry.types
                            , viewNameTitle entry.speciesName
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
                            |> L.map (\attack -> viewMoveWithPvPoke model.moves entry attack)
                            |> (::) (span [ class "mr-2" ] [ text "Fast:" ])
                            |> div [ class "flex flex-row flex-wrap items-center ml-1 mb-2" ]
                        , entry.charged
                            |> L.map (\attack -> viewMoveWithPvPoke model.moves entry attack)
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
                    [ ppTypes entry.types
                    , viewNameTitle entry.speciesName
                    , if model.debug then
                        small [ class "text-xs" ] [ text <| calcTeamScores model league speciesId ]

                      else
                        text ""
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
                    AL.get attackTp effectiveness
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
        |> L.filterMap extractSpeciesId
        |> summariseTeam2 model.moves league.myPokemon


calcTeamScores : Model -> League -> String -> String
calcTeamScores model league opName =
    let
        mkItemInner speciesId pokemon =
            Helpers.evaluateBattle model.pokedex model.moves speciesId pokemon opName
                |> Result.toMaybe

        mkItem : String -> String
        mkItem speciesId =
            lookupMyPokemon league.myPokemon speciesId
                |> Maybe.map (mkItemInner speciesId)
                |> Maybe.andThen identity
                |> Maybe.map ppFloat
                |> Maybe.withDefault "."
    in
    [ league.team.cand1
    , league.team.cand2
    , league.team.cand3
    ]
        |> L.filterMap (extractSpeciesId >> Maybe.map mkItem)
        |> String.join ", "


summariseTeam2 : Dict String MoveType -> Dict String Pokemon -> List String -> List ( String, PType )
summariseTeam2 attacks myPokemon team =
    team
        |> L.filterMap (\speciesId -> Dict.get speciesId myPokemon |> Maybe.map (Tuple.pair speciesId))
        |> Dict.fromList
        |> summariseTeamInner attacks


lookupMyPokemon : Dict String Pokemon -> String -> Maybe Pokemon
lookupMyPokemon myPokemon speciesId =
    Dict.get speciesId myPokemon


summariseTeamInner : Dict String MoveType -> Dict String Pokemon -> List ( String, PType )
summariseTeamInner attacks pokemons =
    let
        createItem : String -> ( String, String ) -> Maybe ( String, PType )
        createItem name ( attack, _ ) =
            Dict.get attack attacks
                |> Maybe.map (\{ type_ } -> ( name, type_ ))

        go : String -> Pokemon -> List ( String, PType ) -> List ( String, PType )
        go speciesId p acc =
            p.charged
                |> Set.toList
                |> L.map (\atk -> ( atk, "**" ))
                |> (::) ( p.fast, "*" )
                |> L.filterMap (createItem speciesId)
                |> (++) acc
    in
    Dict.foldl go [] pokemons



-- -------------------
-- View pokemon
-- -------------------


viewNameTitle : String -> Html msg
viewNameTitle name =
    h3 [ class "text-xl font-bold truncate ml-1" ] [ text name ]


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
    , hr [] []
    , viewTypes (\_ f -> f < 0.9) effectivenesses "Resists"
    ]


{-| returns a dictionary of attack effectiveness against the list of types
-}
getDefenceMeta : Effectiveness -> List PType -> AL.Dict PType Float
getDefenceMeta effectiveness tps =
    let
        go _ dict =
            L.foldl (\tp acc -> AL.get tp dict |> Maybe.withDefault 1 |> (*) acc) 1 tps
    in
    effectiveness
        |> AL.map go


viewTypes : (PType -> Float -> Bool) -> AL.Dict PType Float -> String -> Html msg
viewTypes fn moves title =
    let
        ( normals, supers ) =
            moves
                |> AL.filter fn
                |> AL.toList
                |> L.partition (\( _, v ) -> v > 0.5 && v < 1.5)
    in
    div [ class "flex flex-row items-center mb-2" ]
        [ span [ class "mr-3" ] [ text title ]
        , div [ class "badge-list flex flex-col" ]
            [ supers |> L.map (\( tp, _ ) -> span [ class "super mr-3" ] [ ppType tp ]) |> div [ class "flex flex-row flex-wrap" ]
            , normals |> L.map (\( tp, _ ) -> span [ class "mr-1" ] [ ppType tp ]) |> div [ class "flex flex-row flex-wrap" ]
            ]
        ]


attackToType : Dict String MoveType -> String -> Maybe PType
attackToType attacks attack =
    if String.startsWith "HIDDEN" attack then
        -- we don't show hidden type because there are 10 of them!
        Nothing

    else
        attacks
            |> Dict.get attack
            |> Maybe.map .type_


viewMoveWithPvPoke : Dict String MoveType -> PokedexEntry -> String -> Html msg
viewMoveWithPvPoke attacks entry attack =
    let
        isRec =
            L.member attack entry.recMoves

        isElite =
            L.member attack entry.elite
    in
    case Dict.get attack attacks of
        Just mt ->
            span
                [ style "background-color" <| Tuple.second <| stringFromPType mt.type_
                , class "flex flex-row items-center badge p-1 rounded text-sm"
                ]
                [ ifThenElse isRec pvpPokeLogo (text "")
                , span [ class "truncate" ] [ text <| ifThenElse isElite "*" "" ++ mt.name ]
                ]

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
    div [ class "chooser-container flex flex-row justify-between" ]
        [ Autocomplete.view viewConfig autocomplete choices search
            |> Html.map ACMsg
        , span [ onClick <| SetAutoComplete NoChooser ] [ matIcon "close" ]
        ]


viewChooserPlaceholder : SearchTool -> Html Msg
viewChooserPlaceholder chooser =
    div
        [ class "flex flex-row cursor-pointer"
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


toggleBtn : msg -> Bool -> Html msg
toggleBtn msg expanded =
    button
        [ class "toggle"
        , onClick msg
        ]
        [ matIcon <| ifThenElse expanded "chevron-down" "chevron-right" ]


cardClass : String
cardClass =
    "rounded overflow-hidden shadow-lg p-1 bg-white"


pvpPokeLogo : Html msg
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
    let
        base =
            types |> L.map (\tp -> span [ class "ml-1 mr-1" ] [ ppTypeShort tp ])

        placeholder =
            span [ class "flex flex-row items-center justify-center badge round small ml-1 mr-1" ] []
    in
    div [ class "flex flex-row items-center" ] <|
        if L.length types == 1 then
            base ++ [ placeholder ]

        else
            base


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
    span
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


matIcon : String -> Html msg
matIcon t =
    span [ class <| "mdi mdi-" ++ t ] []


ppFloat : Float -> String
ppFloat =
    FormatNumber.format { usLocale | decimals = Exact 1 }



-- -----------------
-- Commands
-- -----------------


getPokedex : Cmd Msg
getPokedex =
    let
        dec =
            Decode.map2 Tuple.pair
                (Decode.field "moves" decodeMoves)
                (Decode.field "pokemon" decodePokedex)
    in
    Http.get
        { url = "gamemaster.json"
        , expect = Http.expectJson OnPokedex dec
        }


getRankings : Season -> Cmd Msg
getRankings season =
    let
        get n =
            Http.get
                { url = n
                , expect = Http.expectJson (OnRankingData season) decodeRankings
                }
    in
    case season of
        Great ->
            get "rankings-1500.json"

        Ultra ->
            get "rankings-2500.json"

        Master ->
            get "rankings-10000.json"



-- Main program


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "Pokemon PVP team manager"
                , body = [ view m ]
                }
        , subscriptions = \_ -> Sub.none
        }
