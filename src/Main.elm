module Main exposing (main)

import Array exposing (Array)
import Array.Extra as AE
import AssocList as Dict exposing (Dict)
import Autocomplete exposing (..)
import Browser
import Helpers exposing (addScoresToLeague, calculateEffectiveness, evaluateTeam)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder, Value)
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
                model_ =
                    { defaultModel
                        | season = flags.persisted.season
                        , pokedex = flags.pokedex
                        , attacks = Dict.union flags.fast flags.charged
                    }
            in
            ( { model_
                | great = addScoresToLeague model_ flags.persisted.great
                , ultra = addScoresToLeague model_ flags.persisted.ultra
                , master = addScoresToLeague model_ flags.persisted.master
              }
            , Cmd.none
            )

        Err err ->
            Debug.todo (Decode.errorToString err)



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
    | SetChooser PokedexChooser
      -- My pokemons
    | ToggleExpanded Int
    | SelectFastMove Int String
    | SelectChargedMove Int String
    | RemovePokemon Int
      -- middle
    | SelectCandidate Pokemon -- first part of adding to team
    | UpdateTeam Team -- second part
      -- My opponents
    | RemoveOpponent String


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        SwitchSeason season ->
            { model | season = season } |> andPersist

        SwitchPage page ->
            ( { model | page = page }, Cmd.none )

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
                                    { l | opponents = name :: l.opponents }
                            )
            in
            { newModel | chooser = mapSearch (\_ -> "") model.chooser } |> andPersist

        SetChooser chooser ->
            ( { model | chooser = chooser }, Cmd.none )

        ToggleExpanded idx ->
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
            model
                |> updateLeague (\l -> { l | myPokemon = AE.removeAt idx l.myPokemon })
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

        RemoveOpponent name ->
            model
                |> updateLeague (\l -> { l | opponents = L.filter ((/=) name) l.opponents })
                |> andPersist


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
    ( model
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
        mkButton msg txt =
            button
                [ class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded ml-3"
                , onClick msg
                ]
                [ text txt ]

        league =
            case model.season of
                Great ->
                    model.great

                Ultra ->
                    model.ultra

                Master ->
                    model.master

        _ =
            Debug.log "" <| Helpers.evaluateTeams league
    in
    div [ class "m-3" ]
        [ div [ class "flex flex-row justify-between" ]
            [ h1 [ class "text-2xl" ] [ text "Pokemon" ]
            , div []
                [ mkButton (SwitchSeason Great) "Great"
                , mkButton (SwitchSeason Ultra) "Ultra"
                , mkButton (SwitchSeason Master) "Master"
                ]
            , if model.page == Choosing then
                mkButton (SwitchPage Battling) "Battling"

              else
                mkButton (SwitchPage Choosing) "Choosing"
            ]
        , if model.page == Choosing then
            div [ class "main choosing flex flex-row" ]
                [ div [ class "my-pokemon flex flex-col flex-grow" ] (viewMyPokemons model league)
                , div [ class "my-team flex flex-col flex-grow ml-3 mr-3" ] (viewTeam model league)
                , div [ class "opponents flex flex-col flex-grow" ] (viewOpponents model league)
                ]

          else
            div [ class "main battling flex flex-row" ]
                [ div [ class "my-team flex flex-col flex-shrink-0 ml-2 mr-2" ] (viewTeam model league)
                , div [ class "opponents flex flex-col flex-grow" ] (viewOpponentsBattling model league)
                ]
        ]



-- -------------------
-- LHS My Pokemon
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
                Just meta ->
                    viewMyPokemon model meta idx pokemon

                Nothing ->
                    div [ class <| cardClass ++ " mb-2 relative bg-red-200" ]
                        [ div [ class "flex flex-row items-center justify-between" ]
                            [ div [ class "flex flex-row items-center" ]
                                [ h3 [ class "text-xl font-bold" ] [ text pokemon.name ] ]
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
viewMyPokemon model meta idx pokemon =
    let
        mainCls =
            if Maybe.map .name model.selectedPokemon == Just pokemon.name then
                cardClass ++ " mb-2 relative bg-blue-100"

            else
                cardClass ++ " mb-2 relative bg-white"

        viewAttack_ selectMove isSelected attack =
            let
                cls c =
                    class <| "ml-1 rounded border-2 " ++ c
            in
            span
                [ cls <|
                    if isSelected attack then
                        "border-teal-300"

                    else
                        "bg-gray-200"
                , onClick <| selectMove idx attack
                ]
                [ viewAttackBadge model.attacks attack ]

        attacks =
            [ meta.fast
                |> L.map (viewAttack_ SelectFastMove ((==) pokemon.fast))
                |> (::) (text "Fast: ")
                |> div [ class "flex flex-row flex-wrap" ]
            , meta.charged
                |> L.map (viewAttack_ SelectChargedMove (\atk -> Set.member atk pokemon.charged))
                |> (::) (text "Charged: ")
                |> div [ class "flex flex-row flex-wrap" ]
            ]

        topLine =
            div [ class "flex flex-row items-center justify-between" ]
                [ div [ class "flex flex-row items-center" ]
                    [ span [ onClick <| ToggleExpanded idx ]
                        [ if pokemon.expanded then
                            matIcon "chevron-down"

                          else
                            matIcon "chevron-right"
                        ]
                    , h3
                        [ class "text-xl font-bold"
                        , onClick <| SelectCandidate pokemon
                        ]
                        [ text <| pokemon.name ]
                    , meta.types
                        |> L.map (\tp -> span [ class "ml-2" ] [ ppType tp ])
                        |> div []
                    ]
                , deleteIcon <| RemovePokemon idx
                ]
    in
    div [ class mainCls ] <|
        if pokemon.expanded then
            topLine :: attacks

        else
            [ topLine ]



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

        lookup : Maybe String -> Result String Pokemon
        lookup name =
            league.myPokemon
                |> Array.filter (\item -> Just item.name == name)
                |> Array.get 0
                |> Result.fromMaybe ("Could not lookup: " ++ Maybe.withDefault "" name)

        viewMbCand updater mbCand =
            let
                content =
                    mbCand
                        |> lookup
                        |> Result.andThen
                            (\pokemon ->
                                model.pokedex
                                    |> Dict.get pokemon.name
                                    |> Maybe.map (convertToPokedex pokemon)
                                    |> Result.fromMaybe ("Could not look up " ++ pokemon.name ++ " in pokedex")
                            )
                        |> Result.map (\( name, entry ) -> viewWithStrengths model name entry)
                        |> RE.extract (\err -> [ text err ])
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

        score =
            Result.map3 (\a b c -> evaluateTeam ( a, b, c )) (lookup team.cand1) (lookup team.cand2) (lookup team.cand3)
                |> Result.map (Helpers.summariseTeam >> ppFloat)
                |> RE.extract identity
    in
    [ h2 [] [ text "My Team" ]
    , div [] [ text <| "team score: " ++ score ]
    , viewMbCand (\c -> UpdateTeam { team | cand1 = Just c }) team.cand1
    , viewMbCand (\c -> UpdateTeam { team | cand2 = Just c }) team.cand2
    , viewMbCand (\c -> UpdateTeam { team | cand3 = Just c }) team.cand3
    ]


{-| Middle is passed a Pokemon, rights is passed a PokedexEntry
-}
viewWithStrengths : Model -> String -> PokedexEntry -> List (Html msg)
viewWithStrengths model name entry =
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
    [ viewNameTitle name
    , if model.page == Choosing then
        attacks |> L.map viewAttk |> div []

      else
        text ""
    ]
        ++ viewPokemonResistsAndWeaknesses model name



-- -------------------
-- RHS Opponents
-- -------------------


viewOpponents : Model -> League -> List (Html Msg)
viewOpponents model league =
    let
        chooser =
            case model.chooser of
                OpponentChooser search state ->
                    viewChooser model.pokedex search state

                _ ->
                    viewChooserPlaceholder <| OpponentChooser "" Autocomplete.empty

        viewOpponent withDelete name entry =
            div [ class "flex flex-row align-items justify-between" ]
                [ viewNameAndTypes name entry
                , if withDelete then
                    deleteIcon <| RemoveOpponent name

                  else
                    text ""
                ]

        viewer ( withDelete, name ) =
            case Dict.get name model.pokedex of
                Just entry ->
                    div [ class <| cardClass ++ " mb-2" ]
                        (viewOpponent withDelete name entry :: viewPokemonResistsAndWeaknesses model name)

                Nothing ->
                    text <| "Could not look up " ++ name

        opponents =
            L.map (Tuple.pair True) league.opponents
                ++ (L.map (Tuple.pair False) <| Array.toList (Array.map .name league.myPokemon))
                |> L.sortBy Tuple.second
    in
    [ h2 [] [ text "Opponents" ]
    , chooser
    , opponents
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
                        div [ class <| "ml-4 p-1 border-solid border-2 " ++ cls ] <|
                            L.map (\( title, pType ) -> colouredBadge pType title) lst
            in
            div [ class <| cardClass ++ " flex flex-row justify-between mb-1" ]
                [ div [ class "flex flex-row" ]
                    [ viewNameTitle name
                    , viewLst "border-green-500" weak
                    ]
                , viewLst "border-red-500" resists
                ]

        viewer name =
            case Dict.get name model.pokedex of
                Just entry ->
                    viewOpponent name entry

                Nothing ->
                    text <| "Could not look up " ++ name

        opNames =
            league.opponents ++ Array.toList (Array.map .name league.myPokemon)
    in
    [ h2 [] [ text "Opponents" ]
    , opNames
        |> L.sort
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
    L.foldl go ( [], [] ) team


summariseTeam : Model -> League -> List ( String, PType )
summariseTeam model league =
    [ league.team.cand1
    , league.team.cand2
    , league.team.cand3
    ]
        |> L.filterMap identity
        |> summariseTeam2 model.attacks league.myPokemon


summariseTeam2 : Dict String MoveType -> Array Pokemon -> List String -> List ( String, PType )
summariseTeam2 attacks myPokemon team =
    let
        lookup : String -> Maybe Pokemon
        lookup name =
            myPokemon
                |> Array.filter (\item -> item.name == name)
                |> Array.get 0
    in
    team
        |> L.filterMap lookup
        |> summariseTeamInner attacks


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


deleteIcon : Msg -> Html Msg
deleteIcon msg =
    span
        [ class "btn-delete"
        , onClick msg
        ]
        [ matIcon "delete-outline" ]



-- -------------------
-- View pokemon
-- -------------------


ppFloat x =
    round (10 * x)
        |> toFloat
        |> (\y -> String.fromFloat (y / 10))


viewNameAndTypes : String -> PokedexEntry -> Html msg
viewNameAndTypes name entry =
    div [ class "flex flex-row items-center" ]
        [ viewNameTitle name
        , entry.types |> L.map (\tp -> span [ class "ml-1" ] [ ppType tp ]) |> div []
        ]


viewNameTitle name =
    h3 [ class "text-2xl font-bold" ] [ text name ]


viewPokemonResistsAndWeaknesses : Model -> String -> List (Html msg)
viewPokemonResistsAndWeaknesses model name =
    let
        effectivenesses =
            model.pokedex
                |> Dict.get name
                |> Maybe.map .types
                |> Maybe.withDefault []
                |> getDefenceMeta effectiveness
    in
    [ viewTypes (\_ f -> f > 1.1) effectivenesses "Weak to"
    , viewTypes (\_ f -> f < 0.9) effectivenesses "Resists"
    ]


viewTypes fn weaknesses title =
    let
        ( normals, supers ) =
            weaknesses
                |> Dict.filter fn
                |> Dict.toList
                |> L.partition (\( _, v ) -> v > 0.5 && v < 2)
    in
    div [ class "flex flex-row items-center" ]
        [ span [ class "mr-3" ] [ text title ]
        , div [ class "badge-list flex flex-row" ]
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
        , span [ onClick <| SetChooser NoChooser ] [ matIcon "close" ]
        ]


viewChooserPlaceholder : PokedexChooser -> Html Msg
viewChooserPlaceholder chooser =
    div [ class "flex flex-row justify-between mb-2" ]
        [ text "Add pokemon"
        , span [ onClick <| SetChooser chooser ] [ matIcon "pencil" ]
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


cardClass =
    "rounded overflow-hidden shadow-lg p-1 bg-white"


ppType : PType -> Html msg
ppType pType =
    let
        ( str, col ) =
            stringFromPType pType
    in
    badge col str


colouredBadge : PType -> String -> Html msg
colouredBadge pType str =
    let
        ( _, col ) =
            stringFromPType pType
    in
    span
        [ style "background-color" col
        , class "badge p-1 rounded text-sm"
        ]
        [ text str ]


badge : String -> String -> Html msg
badge col str =
    span
        [ style "background-color" col
        , class "badge p-1 rounded text-sm"
        ]
        [ text str ]


matIcon t =
    span [ class <| "mdi mdi-" ++ t ] []



--


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
