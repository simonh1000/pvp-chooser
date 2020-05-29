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
import Model exposing (..)
import Pokemon exposing (PType, effectiveness, stringFromPType)
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
                        , pokedex = flags.pokedex
                        , attacks = Dict.union flags.fast flags.charged
                        , great = flags.persisted.great
                        , ultra = flags.persisted.ultra
                        , master = flags.persisted.master
                        , debug = flags.debug
                    }
            in
            ( addScores model, Cmd.none )

        Err err ->
            ( { defaultModel | page = FatalError <| Decode.errorToString err }, Cmd.none )


type alias Flags =
    { persisted : Persisted
    , pokedex : Dict String PokedexEntry -- keys by lowercase name
    , fast : Dict String MoveType
    , charged : Dict String MoveType
    , effectiveness : Effectiveness
    , debug : Bool
    }


decodeFlags : Decoder Flags
decodeFlags =
    Decode.succeed Flags
        |> andMap (Decode.field "myPokemon" decodePersisted)
        |> andMap (Decode.field "pokemon" decodePokedex)
        |> andMap (Decode.field "fast" decodeMoves)
        |> andMap (Decode.field "charged" decodeMoves)
        |> andMap (Decode.field "effectiveness" decodeEffectiveness)
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
            { model | season = season } |> andPersist

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

        ACSelect isMyPokemon name ->
            let
                pokemon =
                    { blankPokemon
                        | name = name
                        , fast = model.pokedex |> Dict.get name |> Maybe.andThen (\{ fast } -> L.head fast) |> Maybe.withDefault ""
                        , charged = Set.empty
                    }

                newModel =
                    model
                        |> updateLeague
                            (\l ->
                                if isMyPokemon then
                                    { l | myPokemon = Array.push pokemon l.myPokemon }

                                else
                                    { l | opponents = Opponent False name 1 :: l.opponents }
                            )
            in
            { newModel | chooser = mapSearch (\_ -> "") model.chooser } |> andPersist

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
                                , team = removeFromTeam p.name l.team
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
                mapper op =
                    ifThenElse (op.name == name) { op | expanded = not op.expanded } op
            in
            ( updateLeague (\l -> { l | opponents = L.map mapper l.opponents }) model
            , Cmd.none
            )

        UpdateOpponentFrequency name y ->
            model
                |> updateLeague (\l -> { l | opponents = L.map (\op -> ifThenElse (op.name == name) { op | frequency = op.frequency + y } op) l.opponents })
                |> andPersist

        RemoveOpponent name ->
            model
                |> updateLeague (\l -> { l | opponents = L.filter (\op -> op.name /= name) l.opponents })
                |> andPersist


addScores : Model -> Model
addScores model =
    { model
        | great = addScoresToLeague model model.great
        , ultra = addScoresToLeague model model.ultra
        , master = addScoresToLeague model model.master
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


andPersist : Model -> ( Model, Cmd msg )
andPersist model =
    ( addScores model
    , Ports.toJs { tag = "Persist", payload = encodePersisted model }
    )


updateConfig : Bool -> UpdateConfig Msg String
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


getRelevantChoices : String -> Dict String PokedexEntry -> List String
getRelevantChoices search pokedex =
    let
        search_ =
            String.toLower search
    in
    pokedex
        |> Dict.keys
        |> L.filter (\name -> String.contains search_ (String.toLower name))


toId =
    identity



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
    in
    div [ class "h-screen flex flex-col" ]
        [ pvpHeader model.page
        , case model.page of
            Registering ->
                div [ cls "choosing" ]
                    [ div [ class "my-pokemon flex flex-col flex-grow" ] (viewMyPokemons model league)
                    , div [ class "my-team flex flex-col flex-grow ml-2 mr-2" ] (viewTeam model league)
                    , div [ class "opponents flex flex-col flex-grow" ] (viewOpponentsChoosing model league)
                    ]

            TeamOptions ->
                div [ cls "teams" ]
                    [ div [ class "my-pokemon flex flex-col flex-grow" ] (viewTeamOptions model league)
                    , div [ class "my-team flex flex-col flex-grow ml-2 mr-2" ] (viewTeam model league)
                    , div [ class "opponents flex flex-col flex-grow" ] (viewOpponentsBattling model league)
                    ]

            Battling ->
                div [ cls "battling" ]
                    [ div [ class "my-team flex flex-col mr-2" ] (viewTeam model league)
                    , div [ class "opponents flex flex-col flex-grow" ] (viewOpponentsBattling model league)
                    ]

            FatalError string ->
                div [ cls "error" ]
                    [ h1 [] [ text "Fatal Error" ]
                    , div [] [ text string ]
                    ]
        , pvpFooter model.season
        ]


pvpHeader : Page -> Html Msg
pvpHeader tgt =
    header [ class "flex flex-row justify-between p-3 bg-gray-400" ]
        [ h1 [ class "text-2xl justify-center" ] [ text "Pokemon" ]
        , mkRadioButtons
            [ ( SwitchPage Registering, "Registering", Registering == tgt )
            , ( SwitchPage TeamOptions, "Team options", TeamOptions == tgt )
            , ( SwitchPage Battling, "Battling", Battling == tgt )
            ]
        ]


pvpFooter : Season -> Html Msg
pvpFooter tgt =
    footer [ class "flex flex-row items-center justify-end p-3 bg-gray-400" ]
        [ mkRadioButtons
            [ ( SwitchSeason Great, "Great", Great == tgt )
            , ( SwitchSeason Ultra, "Ultra", Ultra == tgt )
            , ( SwitchSeason Master, "Master", Master == tgt )
            ]
        ]


mkRadioButtons : List ( msg, String, Bool ) -> Html msg
mkRadioButtons list =
    let
        cls selected =
            if selected then
                "inline-block border border-blue-500 rounded py-1 px-3 bg-blue-500 text-white"

            else
                "inline-block border border-white rounded hover:border-gray-200 text-blue-500 hover:bg-gray-200 py-1 px-3"

        mkButton ( msg, txt, selected ) =
            li [ class "mr-3" ]
                [ button
                    [ class <| cls selected
                    , onClick msg
                    ]
                    [ text txt ]
                ]
    in
    list
        |> L.map mkButton
        |> ul [ class "flex" ]



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
                    viewChooserPlaceholder <| MyChooser "" Autocomplete.empty

        viewer : Int -> Pokemon -> Html Msg
        viewer idx pokemon =
            case Dict.get pokemon.name model.pokedex of
                Just entry ->
                    viewMyPokemon model entry idx pokemon

                Nothing ->
                    div [ class <| cardClass ++ " mb-2 bg-red-200" ]
                        [ div [ class "flex flex-row items-center justify-between" ]
                            [ viewNameTitle pokemon.name
                            , deleteIcon <| RemovePokemon idx
                            ]
                        , div [] [ text <| "viewMyPokemons: no meta!" ]
                        ]
    in
    [ h2 [] [ text "My Pokemons" ]
    , chooser
    , league.myPokemon
        |> Array.indexedMap viewer
        |> Array.toList
        |> div []
    ]


viewMyPokemon : Model -> PokedexEntry -> Int -> Pokemon -> Html Msg
viewMyPokemon model entry idx pokemon =
    let
        mainCls =
            if Maybe.map .name model.selectedPokemon == Just pokemon.name then
                cardClass ++ " mb-2 bg-blue-100"

            else
                cardClass ++ " mb-2 bg-white"

        viewAttack_ selectMove isSelected attack =
            let
                cls c =
                    class <| "ml-1 cursor-pointer rounded border-4 " ++ c
            in
            div
                [ cls <|
                    if isSelected attack then
                        "border-teal-300"

                    else
                        "border-transparent"
                , onClick <| selectMove idx attack
                ]
                [ viewAttackBadge model.attacks attack ]

        attacks =
            [ entry.fast
                |> L.map (viewAttack_ SelectFastMove ((==) pokemon.fast))
                |> (::) (text "Fast: ")
                |> div [ class "flex flex-row flex-wrap ml-1" ]
            , entry.charged
                |> L.map (viewAttack_ SelectChargedMove (\atk -> Set.member atk pokemon.charged))
                |> (::) (text "Charged: ")
                |> div [ class "flex flex-row flex-wrap" ]
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
                        [ text <| pokemon.name ]
                    ]
                , div [ class "flex flex-row items-center" ]
                    [ entry.types
                        |> L.map (\tp -> span [ class "ml-2" ] [ ppType tp ])
                        |> div []
                    , deleteIcon <| RemovePokemon idx
                    ]
                ]
    in
    div [ class mainCls ] <|
        if pokemon.expanded then
            topLine :: attacks

        else
            [ topLine ]



-- -------------------
-- LHS Teams
-- -------------------


viewTeamOptions : Model -> League -> List (Html Msg)
viewTeamOptions _ league =
    let
        viewOption : ( ( String, String, String ), Float ) -> Html Msg
        viewOption ( ( c1, c2, c3 ), score ) =
            let
                selected =
                    hasMember c1 league.team && hasMember c2 league.team && hasMember c3 league.team
            in
            div
                [ classList
                    [ ( cardClass, True )
                    , ( "mb-1 flex flex-row justify-between", True )
                    , ( "bg-blue-100", selected )
                    ]
                , onClick <| SetTeam ( c1, c2, c3 )
                ]
                [ span [] [ [ c1, c2, c3 ] |> String.join ", " |> text ]
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
            ( pokemon.name, { pokedex | fast = [ pokemon.fast ], charged = Set.toList pokemon.charged } )

        lookupTeamMember : TeamMember -> Result String Pokemon
        lookupTeamMember teamMember =
            case teamMember of
                Unset ->
                    Err "Team member not chosen"

                Chosen name ->
                    lookupName league.myPokemon name

                Pinned name ->
                    lookupName league.myPokemon name

        viewMbCand updater mbCand =
            let
                handler name isPinned =
                    lookupName league.myPokemon name
                        |> Result.andThen
                            (\pokemon ->
                                model.pokedex
                                    |> Dict.get pokemon.name
                                    |> Maybe.map (convertToPokedex pokemon)
                                    |> Result.fromMaybe ("Could not look up " ++ pokemon.name ++ " in pokedex")
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
                        , onClick (updater selected.name)
                        ]
                        content

                Nothing ->
                    div [ class "drop-zone p-1 mb-2 border-gray-500" ]
                        content

        sumFreqs =
            Helpers.calcWeightedTotal league.opponents

        mbScore =
            Result.map3 (\a b c -> evaluateTeam ( a, b, c )) (lookupTeamMember team.cand1) (lookupTeamMember team.cand2) (lookupTeamMember team.cand3)
                |> Result.map (Helpers.summariseTeam league.opponents)
                |> Result.map (\x -> x / sumFreqs)
                |> Result.map ppFloat
    in
    [ h2 [] [ text "My Team" ]
    , viewMbCand (\c -> UpdateTeam { team | cand1 = Chosen c }) team.cand1
    , viewMbCand (\c -> UpdateTeam { team | cand2 = Chosen c }) team.cand2
    , viewMbCand (\c -> UpdateTeam { team | cand3 = Chosen c }) team.cand3
    , mbScore
        |> Result.map (\score -> div [] [ text <| "team score: " ++ score ])
        |> Result.withDefault (text "")
    ]


viewTeamMember : Model -> String -> PokedexEntry -> Bool -> List (Html Msg)
viewTeamMember model name entry isPinned =
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
    [ div [ class "flex flex-row justify-between" ]
        [ viewNameTitle name
        , button [ onClick <| PinTeamMember name ]
            [ matIcon <| ifThenElse isPinned "bookmark" "bookmark-outline" ]
        ]
    , if model.page == Registering then
        attacks |> L.map viewAttk |> div []

      else
        text ""
    ]
        ++ viewPokedexResistsAndWeaknesses entry



-- -------------------
-- RHS Opponents
-- -------------------


viewOpponentsChoosing : Model -> League -> List (Html Msg)
viewOpponentsChoosing model league =
    let
        chooser =
            case model.chooser of
                OpponentChooser search state ->
                    viewChooser model.pokedex search state

                _ ->
                    viewChooserPlaceholder <| OpponentChooser "" Autocomplete.empty

        viewOpponent op entry =
            let
                headerRow =
                    div [ class "flex flex-row align-items justify-between" ]
                        [ div
                            [ class "flex flex-row items-center"
                            ]
                            [ toggleBtn (ToggleOpponent op.name) op.expanded
                            , viewNameTitle op.name
                            ]
                        , div [ class "flex flex-row items-center" ]
                            [ entry.types |> L.map (\tp -> span [ class "ml-1" ] [ ppType tp ]) |> div []
                            , button [ onClick <| UpdateOpponentFrequency op.name -1, class "ml-2 mr-1" ] [ text "-" ]
                            , span [ class "mr-1" ] [ text <| String.fromInt op.frequency ]
                            , button [ onClick <| UpdateOpponentFrequency op.name 1, class "mr-1" ] [ text "+" ]
                            , deleteIcon <| RemoveOpponent op.name
                            ]
                        ]

                content =
                    if op.expanded then
                        viewPokemonResistsAndWeaknesses model op.name

                    else
                        []
            in
            div [ class <| cardClass ++ " mb-2" ]
                (headerRow :: content)

        viewer op =
            case Dict.get op.name model.pokedex of
                Just entry ->
                    viewOpponent op entry

                Nothing ->
                    text <| "Could not look up " ++ op.name
    in
    [ h2 [] [ text "Opponents" ]
    , chooser
    , league.opponents
        |> L.sortBy .name
        |> L.map viewer
        |> div []
    ]


viewOpponentsBattling : Model -> League -> List (Html Msg)
viewOpponentsBattling model league =
    let
        team : List ( String, PType )
        team =
            summariseTeam model league

        viewOpponent name entry =
            let
                ( weak, resists ) =
                    checkAttackAgainstDefenderType effectiveness team entry.types

                viewLst cls lst =
                    if L.isEmpty lst then
                        text ""

                    else
                        div [ class <| "flex flex-row flex-wrap items-baseline ml-4 p-1 " ++ cls ] <|
                            L.map (\( title, pType ) -> colouredBadge pType title) lst
            in
            div [ class <| cardClass ++ " flex flex-row  items-center justify-between mb-1" ]
                [ div [ class "flex flex-row items-center" ]
                    [ div []
                        [ viewNameTitle name
                        , if model.debug then
                            small [ class "text-xs" ] [ text <| calcTeamScores model league name ]

                          else
                            text ""
                        ]
                    , viewLst "bg-green-200" weak
                    ]
                , viewLst "bg-red-200" resists
                ]

        viewer { name } =
            case Dict.get name model.pokedex of
                Just entry ->
                    viewOpponent name entry

                Nothing ->
                    text <| "Could not look up " ++ name
    in
    [ h2 [] [ text "Opponents" ]
    , league.opponents
        |> L.sortBy (.frequency >> (*) -1)
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
        |> Array.filter (\item -> item.name == name)
        |> Array.get 0


summariseTeamInner : Dict String MoveType -> List Pokemon -> List ( String, PType )
summariseTeamInner attacks pokemons =
    let
        createItem : String -> ( String, String ) -> Maybe ( String, PType )
        createItem name ( attack, suffix ) =
            Dict.get attack attacks
                |> Maybe.map (\{ type_ } -> ( name ++ " " ++ suffix, type_ ))

        go : Pokemon -> List ( String, PType ) -> List ( String, PType )
        go p acc =
            p.charged
                |> Set.toList
                |> L.map (\atk -> ( atk, "**" ))
                |> (::) ( p.fast, "*" )
                |> L.filterMap (createItem p.name)
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


attackToType attacks attack =
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
    let
        col =
            attacks
                |> Dict.get attack
                |> Maybe.map (.type_ >> stringFromPType >> Tuple.second)
                |> Maybe.withDefault "#ffffff"
    in
    badge col attack



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
    div [ class "flex flex-row justify-between mb-2" ]
        [ text "Add pokemon"
        , span [ onClick <| SetAutoComplete chooser ] [ matIcon "pencil" ]
        ]


viewConfig : Autocomplete.ViewConfig String
viewConfig =
    let
        customizedLi keySelected mouseSelected item =
            { attributes =
                [ classList
                    [ ( "autocomplete-item", True )
                    , ( "key-selected", keySelected )
                    , ( "mouse-selected", mouseSelected )
                    ]
                ]
            , children = [ Html.text item ]
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


deleteIcon : Msg -> Html Msg
deleteIcon msg =
    span
        [ class "btn-delete ml-1"
        , onClick msg
        ]
        [ matIcon "delete-outline" ]


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
        , class "flex flex-row items-center justify-center badge round"
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
