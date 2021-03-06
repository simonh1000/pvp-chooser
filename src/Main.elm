module Main exposing (main, shadow, viewAttacksWithRecommendations)

import AssocList as AL
import Autocomplete exposing (..)
import Browser
import Common.CoreHelpers exposing (addCmd, ifThenElse, rejectByList)
import Dict exposing (Dict)
import FormatNumber
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import Helpers exposing (addScoresToLeague, calculateEffectiveness, evaluateTeam, lookupId)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Extra exposing (andMap)
import List as L
import Maybe.Extra as ME
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
                        | season = flags.persisted.season
                        , leagues = flags.persisted.leagues
                        , debug = flags.debug
                    }

                page =
                    if Dict.isEmpty model.leagues then
                        Intro

                    else
                        LoadingDex
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
    = SwitchPage Bool Page
    | SwitchSeason SeasonName
      -- Autocomplete
    | ACMsg Autocomplete.Msg
    | ACSearch String
    | ACSelect Bool String -- isMyPokemon (or is selecting an opponent) name
    | SetAutoComplete SearchTool
      -- Registering: My pokemon
    | UpdateSearch String
    | ToggleMyPokemon String
    | SelectFastMove String String
    | SelectChargedMove String String
    | RemovePokemon String
      -- Registering: Team
    | UpdateSeasonToCopy SeasonName
    | CopyFromSeason SeasonName
    | UpdateTeam Team -- second part
    | SwapTeam Bool -- isTopTwo
      -- Team chooser
    | UpdateTeamOptionsSearch String
    | PinTeamMember String
      -- Registering: opponents
    | ToggleOpponent String
    | UpdateOpponentFrequency String Int -- name
    | RemoveOpponent String
      --
    | OnPokedex (Result Http.Error ( Dict String MoveType, Pokedex ))
    | OnRankingData SeasonName (Result Http.Error (Dict String RankingEntry))


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        SwitchPage overRideIntro page ->
            ( switchPage page { model | page = ifThenElse overRideIntro registerPage model.page }
            , Cmd.none
            )

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
                    getRelevantChoices model.season search model.pokedex
                        |> Autocomplete.update (updateConfig isMyPokemon) msg autocomplete

                ( chooser, c ) =
                    case model.chooser of
                        OpponentChooser search autocomplete ->
                            handler False search autocomplete
                                |> Tuple.mapFirst (OpponentChooser search)

                        _ ->
                            ( model.chooser, Nothing )

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
                    case ( model.page, isMyPokemon ) of
                        ( Registering m, False ) ->
                            Registering <| { m | opponents = speciesId :: m.opponents }

                        _ ->
                            model.page
            in
            { newModel
                | chooser = resetSearch model.chooser
                , page = page
            }
                |> andPersist

        SetAutoComplete chooser ->
            ( { model | chooser = chooser }, Cmd.none )

        -- My Pokemon
        UpdateSearch search ->
            ( { model | chooser = MyChooser search }, Cmd.none )

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

        UpdateSeasonToCopy seasonName ->
            ( { model | page = mapRegistering (\m -> { m | seasonToCopy = seasonName }) model.page }, Cmd.none )

        CopyFromSeason seasonName ->
            case Dict.get (serialiseSeason seasonName) model.leagues of
                Just leagueToCopy ->
                    let
                        myPokemon =
                            leagueToCopy.myPokemon
                                |> Dict.filter
                                    (\speciesId _ ->
                                        model.pokedex |> Dict.get speciesId |> Maybe.andThen .score |> ME.isJust
                                    )

                        updater league =
                            { league | myPokemon = myPokemon }
                    in
                    model
                        |> updateLeague updater
                        |> andPersist

                _ ->
                    ( model, Cmd.none )

        UpdateTeamOptionsSearch search ->
            ( updateTeamOptionsSearch search model, Cmd.none )

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
                |> updateTeamOptionsSearch ""
                |> andPersist

        UpdateTeam team ->
            -- mapRegistering has no effect on TeamChoosing page
            model
                |> updateLeague (\l -> { l | team = team })
                |> andPersist

        -- team chooser
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
                    ( { model
                        | moves = moves
                        , pokedex = pokedex
                      }
                        |> switchPage registerPage
                        |> addScores
                    , getRankings model.season
                    )

                Err _ ->
                    ( { model | page = FatalError "Could not load gamemaster data" }, Cmd.none )

        OnRankingData season res ->
            case res of
                Ok rankings ->
                    if season == model.season then
                        ( { model
                            | pokedex = attachRankings rankings model.pokedex
                            , errorMessage = Nothing
                          }
                            -- pre-populate some opponents
                            |> updateLeague (prePopulateOpponents rankings)
                            |> switchPage registerPage
                            |> addScores
                        , Cmd.none
                        )

                    else
                        -- user has changed season - we do not want this data any more
                        ( { model | errorMessage = Nothing }, Cmd.none )

                Err _ ->
                    ( { model | errorMessage = Just "Could not access ranking data" }
                    , Cmd.none
                    )


updateTeamOptionsSearch : String -> Model -> Model
updateTeamOptionsSearch search model =
    let
        page =
            case model.page of
                TeamOptions _ ->
                    TeamOptions search

                _ ->
                    model.page
    in
    { model | page = page }


switchPage : Page -> Model -> Model
switchPage page model =
    let
        newPage =
            case ( page, model.page ) of
                ( _, Intro ) ->
                    model.page

                ( _, FatalError _ ) ->
                    model.page

                ( Registering _, _ ) ->
                    let
                        opponents =
                            model |> getCurrentLeague |> .opponents |> sortOpponents
                    in
                    Registering { blankRegistering | opponents = opponents }

                _ ->
                    page
    in
    { model | page = newPage }


andPersist : Model -> ( Model, Cmd msg )
andPersist model =
    ( addScores model
    , Ports.persist <| encodePersisted model
    )


prePopulateOpponents : Dict String RankingEntry -> League -> League
prePopulateOpponents rankings league =
    if Dict.isEmpty league.opponents then
        let
            opponents =
                rankings
                    |> Dict.filter (\k _ -> not <| String.contains "shadow" k)
                    |> Dict.toList
                    |> L.sortBy (Tuple.second >> .score >> (*) -1)
                    |> L.take 5
                    |> L.map (\( k, _ ) -> ( k, blankOpponent ))
                    |> Dict.fromList
        in
        { league | opponents = opponents }

    else
        league


addScores : Model -> Model
addScores model =
    { model | leagues = Dict.map (\_ -> addScoresToLeague model) model.leagues }


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


getRelevantChoices : SeasonName -> String -> Dict String PokedexEntry -> List ( String, PokedexEntry )
getRelevantChoices season search pokedex =
    let
        search_ =
            String.toLower search

        filterFn =
            if L.member season [ UltraPremier, MasterPremier ] then
                \_ entry -> not (Set.member "mythical" entry.tags || Set.member "legendary" entry.tags) && String.contains search_ (String.toLower entry.speciesName)

            else
                \_ entry -> String.contains search_ (String.toLower entry.speciesName)
    in
    pokedex
        |> Dict.filter filterFn
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
    in
    div [ class "h-screen flex flex-col" ]
        [ pvpHeader model.season model.page
        , case model.page of
            Intro ->
                div [ class "intro flex-grow p-3" ]
                    [ h2 [] [ text "Introduction" ]
                    , p [] [ text "This app will help you keep track of your pokemon and their types as well as of the competitors you encounter. The data you add is stored on your computer and nowhere else." ]
                    , img [ src "images/screenshot.png", class "mb-2" ] []
                    , p [] [ text "To start, add some of your pokemon, and select their movesets (PvPoke's recommendations are shown in line)." ]
                    , p [] [ text "Then add some of the opponents you encounter - a few of the most common are pre-populated for you - and their relative frequency." ]
                    , p [] [ text "The app then enables you to build teams of three and to compare them. Each team gets a score. The absolute value is meaningless, but the relative scores may help you. The algorithm focuses on type dominance and does not take into account the details of energy generation and usage. Consequently, it is unlikely to recommend an unbalanced team, even though some top players are using them - I reached level 8 in season 1 so don't consider me an expert! YMMV" ]
                    , p [] [ text "A summary page is available while battling - perhaps it will help you choose the right attack in the heat of the moment!" ]
                    , div [] [ mkStyledButton ( SwitchPage True registerPage, "Start", True ) ]
                    ]

            LoadingDex ->
                div [ class "loading flex-grow" ] []

            Registering m ->
                div [ cls "choosing grid grid-cols-1 md:grid-cols-5 gap-2" ] <|
                    viewRegistering model league m

            TeamOptions search ->
                div [ cls "teams grid grid-cols-1 md:grid-cols-5 gap-2" ]
                    [ div [ class "my-pokemon flex flex-col col-span-1" ] (viewTeamLHS model league search)
                    , div [ class "my-team flex flex-col col-span-2" ] (viewTeamMiddle model Nothing league)
                    , div [ class "opponents flex flex-col col-span-2" ] (viewTeamRHS model league)
                    ]

            Battling ->
                div [ cls "battling grid grid-cols-1 md:grid-cols-3 gap-2" ]
                    [ div [ class "my-team flex flex-col" ] (viewTeamMiddle model Nothing league)
                    , div [ class "opponents flex flex-col col-span-2" ] (viewTeamRHS model league)
                    ]

            FatalError string ->
                div [ cls "error" ]
                    [ h1 [] [ text "Fatal Error" ]
                    , div [] [ text string ]
                    ]
        , pageFooter model.errorMessage
        ]


sortOpponents : Dict String Opponent -> List String
sortOpponents opponents =
    opponents
        |> Dict.toList
        |> L.sortBy (Tuple.second >> .frequency >> (*) -1)
        |> L.map Tuple.first


pvpHeader : SeasonName -> Page -> Html Msg
pvpHeader selectedSeason tgt =
    let
        switcher =
            mkRadioButtons
                [ ( SwitchPage False registerPage, "Registering", isRegistering tgt )
                , ( SwitchPage False (TeamOptions ""), "Team options", isTeamOptions tgt )
                , ( SwitchPage False Battling, "Battling", Battling == tgt )
                ]

        title =
            h1 [ class "text-2xl justify-center" ] [ text "Pokemon PVP team manager" ]

        seasonSelector =
            mkSeasonsSelector SwitchSeason selectedSeason seasons
    in
    header [ class "flex flex-row justify-between p-3 bg-gray-400" ] <|
        case tgt of
            Intro ->
                [ title ]

            FatalError _ ->
                [ title ]

            _ ->
                [ title, div [ class "flex flex-row" ] [ switcher, seasonSelector ] ]


mkSeasonsSelector : (SeasonName -> Msg) -> SeasonName -> List SeasonName -> Html Msg
mkSeasonsSelector updateMsg selectedSeason =
    let
        mkSeasonOption season =
            option
                [ value <| serialiseSeason season
                , selected <| season == selectedSeason
                ]
                [ text <| ppSeason season ]
    in
    L.map mkSeasonOption
        >> select
            [ onInput (deserialiseSeason >> updateMsg)
            , class "inline-block border border-blue-500 rounded py-1 px-3 bg-blue-500 text-white"
            ]


pageFooter : Maybe String -> Html Msg
pageFooter errorMessage =
    case errorMessage of
        Just err ->
            footer [ class "flex flex-row items-center justify-between p-3 bg-red-400" ]
                [ text err ]

        Nothing ->
            footer [ class "flex flex-row items-center justify-between p-3 bg-gray-400" ]
                [ span [] [ text "Credits: Meta data from ", a [ href "https://pvpoke.com/" ] [ text "PvPoke" ] ]
                , span [] [ text "Privacy: Uses Google Analytics" ]
                , span [] [ text "Code: ", a [ href "https://github.com/simonh1000/pvp-chooser" ] [ text "Github" ] ]
                , span [] [ text "Feedback: ", a [ href "https://twitter.com/lambda_simon" ] [ text "@lambda_simon" ] ]
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
                "inline-block border rounded py-1 px-3 border-blue-500 bg-blue-500 text-white"

            else
                "inline-block border rounded py-1 px-3 border-white text-blue-500 hover:border-gray-200 hover:bg-gray-200"
    in
    button
        [ class cls
        , onClick msg
        ]
        [ text txt ]



-- -------------------
-- Registering
-- -------------------


viewRegistering : Model -> League -> RegisteringModel -> List (Html Msg)
viewRegistering model league m =
    [ div [ class "pvpoke flex flex-col" ] <| viewRegisteringLHS model league
    , div [ class "my-pokemon col-span-2 flex flex-col" ] (viewRegisteringMiddle model league m.seasonToCopy)
    , div [ class "opponents col-span-2 flex flex-col" ] (viewRegisteringRHS model league m.opponents)
    ]



-- LHS


viewRegisteringLHS : Model -> League -> List (Html Msg)
viewRegisteringLHS model league =
    let
        ppFloat_ =
            Maybe.map ppFloat >> Maybe.withDefault ""

        alreadyUsed =
            Dict.keys league.myPokemon

        mkItem : ( String, PokedexEntry ) -> Html Msg
        mkItem ( speciesId, entry ) =
            let
                requiresEliteMove =
                    L.any (\m -> L.member m entry.elite) entry.recMoves
            in
            li [ class <| cardClass ++ " mb-2 bg-white" ]
                [ div [ class "flex flex-row justify-between" ]
                    [ div [ class "flex flex-row align-center" ]
                        [ button
                            [ onClick <| ACSelect True speciesId
                            , class "toggle"
                            , title "Add to My Pokemon"
                            ]
                            [ matIcon "plus toggle" ]
                        , ppTypes entry.types
                        , span [ class "font-bold" ] (viewName entry.speciesName requiresEliteMove)
                        ]
                    , div [ class "text-sm" ] [ text (ppFloat_ entry.score) ]
                    ]
                ]

        filterRecs search =
            model.pokedex
                |> rejectByList alreadyUsed
                |> Dict.filter (\_ entry -> String.contains (String.toLower search) (String.toLower entry.speciesName))
                |> Dict.toList
                |> L.sortBy (Tuple.second >> .score >> Maybe.withDefault -99 >> (*) -1)
                |> L.take 40
                |> L.map mkItem
                |> ul []

        mkElement htm1 search =
            [ h2 [] [ text <| "PvPoke recommendations" ]
            , div [ class "flex flex-row items-center justify-between mb-2" ] htm1
            , filterRecs search
            ]
    in
    case model.chooser of
        MyChooser search ->
            mkElement
                [ div []
                    [ input [ value search, onInput UpdateSearch ] []
                    , viewCloseChooser
                    ]
                ]
                search

        _ ->
            mkElement
                [ viewChooserPlaceholder <| MyChooser ""
                , pvpPokeLogo
                ]
                ""


shadow : String
shadow =
    "(Shadow)"



--Middle


type alias MyPokemonData =
    { speciesId : String
    , pokemon : Pokemon
    , dex : Maybe PokedexEntry
    }


viewRegisteringMiddle : Model -> League -> SeasonName -> List (Html Msg)
viewRegisteringMiddle model league seasonToCopy =
    let
        addData speciesId pokemon =
            MyPokemonData speciesId pokemon (Dict.get speciesId model.pokedex)

        defaultView { speciesId, pokemon, dex } =
            div [ class <| cardClass ++ " mb-2 bg-red-200" ]
                [ div [ class "flex flex-row items-center justify-between" ]
                    [ viewNameTitle speciesId
                    , deleteIcon <| RemovePokemon speciesId
                    ]
                , div [] [ text <| "Unexpected error looking up. Please delete and re-add" ]
                ]

        viewer : MyPokemonData -> Html Msg
        viewer ({ speciesId, pokemon, dex } as data) =
            Maybe.map (viewMyPokemon model speciesId pokemon) dex
                |> Maybe.withDefault (defaultView data)

        copyPart =
            [ div [ class "mb-3" ]
                [ matIcon "arrow-left-bold"
                , text "First add some of your pokemon from the list to the left"
                ]
            , div [ class "mb-3" ] [ text "Then select the attacks you are using" ]
            , div []
                [ span [ class "mr-1" ] [ text "Or copy from:" ]
                , seasons
                    |> L.filter ((/=) model.season)
                    |> mkSeasonsSelector UpdateSeasonToCopy Great
                , button
                    [ class "inline-block border rounded ml-1 py-1 px-3 border-blue-500 bg-blue-500 text-white"
                    , onClick <| CopyFromSeason seasonToCopy
                    ]
                    [ text "Copy" ]
                ]
            ]

        normalPart =
            league.myPokemon
                |> Dict.map addData
                |> Dict.values
                |> L.sortBy (\{ dex } -> dex |> Maybe.andThen .score |> Maybe.withDefault 0 |> (*) -1)
                |> L.map viewer
    in
    [ h2 [] [ text <| "My Pokemon for " ++ ppSeason model.season ]
    , div [ class "mb-2" ] [ text "\u{00A0}" ]
    ]
        ++ (if Dict.isEmpty league.myPokemon then
                copyPart

            else
                normalPart
           )


viewAttacksWithRecommendations : Dict String MoveType -> PokedexEntry -> String -> Pokemon -> List (Html Msg)
viewAttacksWithRecommendations moves entry speciesId pokemon =
    let
        viewAttack_ selectMove isSelected attack =
            span
                [ class <| "flex flex-row items-center cursor-pointer rounded p-1 " ++ ifThenElse isSelected "bg-green-300" "bg-transparent"
                , onClick <| selectMove speciesId attack
                ]
                [ viewMoveWithPvPoke moves entry attack ]

        entry_ =
            addReturn entry
    in
    [ entry_.fast
        |> L.map (\attack -> viewAttack_ SelectFastMove (attack == pokemon.fast) attack)
        |> (::) (span [ class "attacks-header" ] [ text "Fast: " ])
        |> div [ class "flex flex-row flex-wrap items-center ml-1 " ]
    , entry_.charged
        |> L.map (\attack -> viewAttack_ SelectChargedMove (Set.member attack pokemon.charged) attack)
        |> (::) (span [ class "attacks-header" ] [ text "Charged: " ])
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



-- Right


viewRegisteringRHS : Model -> League -> List String -> List (Html Msg)
viewRegisteringRHS model league names =
    let
        chooser =
            div [ class "flex flex-row items-center justify-between mb-2 " ] <|
                case model.chooser of
                    OpponentChooser search state ->
                        [ viewChooser model.pokedex model.season search state ]

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

                entry_ =
                    addReturn entry

                content =
                    if op.expanded then
                        [ entry_.fast
                            |> L.map (viewMoveWithPvPoke model.moves entry_)
                            |> (::) (span [ class "mr-2" ] [ text "Fast:" ])
                            |> div [ class "flex flex-row flex-wrap items-center ml-1 mb-2" ]
                        , entry_.charged
                            |> L.map (viewMoveWithPvPoke model.moves entry_)
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



-- -------------------
-- Team Options
-- -------------------


viewTeamLHS : Model -> League -> String -> List (Html Msg)
viewTeamLHS model league search =
    let
        viewOption : ( Team, Float ) -> Html Msg
        viewOption ( { cand1, cand2, cand3 } as team, score ) =
            let
                getName cand =
                    cand |> extractSpeciesId |> Maybe.andThen (\id -> Dict.get id model.pokedex) |> Maybe.map .speciesName |> Maybe.withDefault "error"

                title =
                    [ cand1, cand2, cand3 ]
                        |> L.map (\c -> span [ class "truncate flex-1 mr-1" ] [ text <| getName c ])
                        |> span [ class "flex flex-grow overflow-hidden" ]
            in
            div
                [ classList
                    [ ( cardClass, True )
                    , ( "mb-1 flex flex-row justify-between cursor-pointer", True )
                    , ( "bg-blue-100", getTeamList league.team == getTeamList team )
                    ]
                , onClick <| UpdateTeam team
                ]
                [ title
                , span [ class "text-sm" ] [ text <| ppFloat score ]
                ]

        teams =
            if search == "" then
                Helpers.evaluateTeams league

            else
                Helpers.evalTeamsSearch model.pokedex league search
    in
    [ h2 [] [ text "Team options" ]
    , div [ class "flex flex-row items-center mb-2" ]
        [ input
            [ value search
            , onInput UpdateTeamOptionsSearch
            , class "pr-5"
            ]
            []
        , span
            [ onClick <| UpdateTeamOptionsSearch ""
            , style "marginLeft" "-1.1rem"
            ]
            [ matIcon "backspace-outline" ]
        ]
    , teams
        |> L.take 20
        |> L.map viewOption
        |> div [ class "flex flex-col" ]
    ]



-- -------------------
-- Middle: Team
-- -------------------


viewTeamMiddle : Model -> Maybe String -> League -> List (Html Msg)
viewTeamMiddle model selectedPokemon league =
    let
        team =
            league.team

        lookupTeamMember : TeamMember -> Result String Pokemon
        lookupTeamMember =
            extractSpeciesId >> Result.fromMaybe "Team member not chosen" >> Result.andThen (lookupId league.myPokemon)

        viewMbCand updater mbCand =
            let
                handler name isPinned =
                    lookupId league.myPokemon name
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
                    TeamOptions _ ->
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
                Battling ->
                    []

                _ ->
                    viewAttacksWithRecommendations model.moves entry speciesId pokemon
    in
    topLine :: middlePart ++ viewPokedexResistsAndWeaknesses entry


addReturn : PokedexEntry -> PokedexEntry
addReturn entry =
    { entry
        | charged =
            if L.member "RETURN" entry.recMoves then
                entry.charged ++ [ "RETURN" ]

            else
                entry.charged
    }


viewTeamRHS : Model -> League -> List (Html Msg)
viewTeamRHS model league =
    let
        team : List ( String, PType )
        team =
            summariseTeam model league

        mkBadge ( mySpeciesId, pType ) =
            model.pokedex
                |> Dict.get mySpeciesId
                |> Maybe.andThen (.speciesName >> String.split " " >> L.head)
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


{-| Used by Registering and Team
-}
viewMyPokemon : Model -> String -> Pokemon -> PokedexEntry -> Html Msg
viewMyPokemon model speciesId pokemon entry =
    let
        mainCls =
            " mb-2 bg-white"

        rhs1 =
            if pokemon.expanded then
                []

            else
                [ if L.all (\attk -> L.member attk entry.recMoves) (pokemon.fast :: Set.toList pokemon.charged) then
                    pvpPokeLogo

                  else
                    text ""
                , summariseMoves model.moves pokemon
                ]

        rhs2 =
            if pokemon.expanded then
                deleteIcon <| RemovePokemon speciesId

            else
                entry.score |> Maybe.map ppFloat |> Maybe.withDefault "" |> text

        topLine =
            div [ class "flex flex-row items-center justify-between" ]
                [ -- LHS
                  div [ class "flex flex-row items-center" ]
                    [ toggleBtn (ToggleMyPokemon speciesId) pokemon.expanded
                    , ppTypes entry.types
                    , viewNameTitle entry.speciesName
                    ]
                , -- RHS
                  div [ class "flex flex-row items-center text-sm" ] <|
                    (rhs1 ++ [ rhs2 ])

                -- getAttackTypes model.attacks entry
                ]
    in
    div [ class <| cardClass ++ mainCls ] <|
        if pokemon.expanded then
            topLine :: viewAttacksWithRecommendations model.moves entry speciesId pokemon

        else
            [ topLine ]


viewNameTitle : String -> Html msg
viewNameTitle name =
    h3 [ class "text-xl font-bold truncate ml-1" ] <|
        viewName name False


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
    AL.map go effectiveness


viewTypes : (PType -> Float -> Bool) -> AL.Dict PType Float -> String -> Html msg
viewTypes fn moves title =
    let
        ( normals, supers ) =
            moves
                |> AL.filter fn
                |> AL.toList
                |> L.partition (\( _, v ) -> v > 0.5 && v < 1.5)

        content =
            [ supers |> L.map (\( tp, _ ) -> span [ class "super mr-3" ] [ ppType tp ]) |> div [ class "flex flex-row flex-wrap" ]
            , normals |> L.map (\( tp, _ ) -> span [ class "mr-1" ] [ ppType tp ]) |> div [ class "flex flex-row flex-wrap" ]
            ]
    in
    div [ class "flex flex-row items-center mb-2" ]
        [ div [ class "attacks-header mr-1" ] [ text title ]
        , if (L.length <| normals ++ supers) > 7 then
            div [ class "badge-list flex flex-col" ] content

          else
            div [ class "badge-list flex flex-row flex-grow justify-between" ] content
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


viewChooser : Dict String PokedexEntry -> SeasonName -> String -> Autocomplete.State -> Html Msg
viewChooser pokedex season search autocomplete =
    let
        choices =
            getRelevantChoices season search pokedex
    in
    div [ class "chooser-container flex flex-row justify-between" ]
        [ Autocomplete.view viewConfig autocomplete choices search
            |> Html.map ACMsg
        , viewCloseChooser
        ]


viewCloseChooser =
    span [ onClick <| SetAutoComplete NoChooser ] [ matIcon "close" ]


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


viewName : String -> Bool -> List (Html msg)
viewName name requiresEliteMove =
    if String.endsWith "(Shadow)" name then
        [ text <| String.dropRight (String.length shadow) name
        , span [ class "text-purple-400" ] [ matIcon "fire" ]
        , text <| ifThenElse requiresEliteMove "*" ""
        ]

    else
        [ text name
        , text <| ifThenElse requiresEliteMove "*" ""
        ]


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


getRankings : SeasonName -> Cmd Msg
getRankings season =
    Http.get
        { url = getUrl season
        , expect = Http.expectJson (OnRankingData season) decodeRankings
        }



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
