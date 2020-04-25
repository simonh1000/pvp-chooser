module Main exposing (main)

import Array
import Array.Extra as AE
import AssocList as Dict exposing (Dict)
import Autocomplete exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder, Value)
import List as L
import Model exposing (..)
import Pokemon exposing (PType, stringFromPType)
import Ports
import Result.Extra as RE
import Set


init : Value -> ( Model, Cmd Msg )
init value =
    case Decode.decodeValue decodeFlags value of
        Ok flags ->
            let
                currentPokemon =
                    flags.myPokemon.master
            in
            ( { defaultModel
                | myPokemon = currentPokemon.myPokemon
                , team = currentPokemon.team
                , opponents = currentPokemon.opponents
                , great = flags.myPokemon.great
                , ultra = flags.myPokemon.ultra
                , master = flags.myPokemon.master
                , pokedex = flags.pokedex
                , effectiveness = flags.effectiveness
                , attacks = Dict.union flags.fast flags.charged
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
            ( { model | season = season }, Cmd.none )

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
                    if isMyPokemon then
                        { model | myPokemon = Array.push pokemon model.myPokemon }
                        --, autocomplete = resetToFirstItem (updateConfig model.search) (getRelevantChoices model) model.autocomplete

                    else
                        { model | opponents = name :: model.opponents }
            in
            { newModel | chooser = mapSearch (\_ -> "") model.chooser } |> andPersist

        SetChooser chooser ->
            ( { model | chooser = chooser }, Cmd.none )

        ToggleExpanded idx ->
            { model | myPokemon = AE.update idx (\p -> { p | expanded = not p.expanded }) model.myPokemon } |> andPersist

        SelectFastMove idx move ->
            { model | myPokemon = AE.update idx (\p -> { p | fast = move }) model.myPokemon } |> andPersist

        SelectChargedMove idx move ->
            let
                updater charged =
                    if Debug.log "member?" <| Set.member move charged then
                        Set.remove move charged

                    else
                        Set.insert move charged
            in
            { model | myPokemon = AE.update idx (\p -> { p | charged = updater p.charged }) model.myPokemon } |> andPersist

        RemovePokemon idx ->
            { model | myPokemon = AE.removeAt idx model.myPokemon } |> andPersist

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
            { model
                | team = team
                , selectedPokemon = Nothing
            }
                |> andPersist

        RemoveOpponent name ->
            { model | opponents = L.filter ((/=) name) model.opponents } |> andPersist


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
                [ div [ class "my-pokemon flex flex-col flex-grow" ] (viewMyPokemon model league)
                , div [ class "my-team flex flex-col flex-grow ml-3 mr-3" ] (viewTeam model)
                , div [ class "opponents flex flex-col flex-grow" ] (viewOpponents model)
                ]

          else
            div [ class "main battling flex flex-row" ]
                [ div [ class "my-team flex flex-col flex-shrink-0 ml-2 mr-2" ] (viewTeam model)
                , div [ class "opponents flex flex-col flex-grow" ] (viewOpponentsBattling model)
                ]
        ]



-- -------------------
-- LHS My Pokemon
-- -------------------


viewMyPokemon : Model -> League -> List (Html Msg)
viewMyPokemon model league =
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
                    viewPokemon model meta idx pokemon

                Nothing ->
                    text <| "Could not look up " ++ pokemon.name
    in
    [ h2 [] [ text "My Pokemons" ]
    , chooser
    , league.myPokemon
        |> Array.indexedMap viewer
        |> Array.toList
        |> div []
    ]


viewPokemon : Model -> PokedexEntry -> Int -> Pokemon -> Html Msg
viewPokemon model meta idx pokemon =
    let
        mainCls =
            if Maybe.map .name model.selectedPokemon == Just pokemon.name then
                cardClass ++ " relative bg-blue-100"

            else
                cardClass ++ " relative bg-white"

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
                        [ text pokemon.name ]
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


viewTeam : Model -> List (Html Msg)
viewTeam model =
    let
        team =
            model.team

        convertToPokedex : Pokemon -> PokedexEntry -> ( String, PokedexEntry )
        convertToPokedex pokemon pokedex =
            ( pokemon.name, { pokedex | fast = [ pokemon.fast ], charged = Set.toList pokemon.charged } )

        lookup : String -> Maybe Pokemon
        lookup name =
            model.myPokemon
                |> Array.filter (\item -> item.name == name)
                |> Array.get 0

        viewMbCand updater mbCand =
            let
                content =
                    mbCand
                        |> Result.fromMaybe ""
                        |> Result.andThen (\name -> lookup name |> Result.fromMaybe "no match in my pokemon")
                        |> Result.andThen
                            (\pokemon ->
                                model.pokedex
                                    |> Dict.get pokemon.name
                                    |> Maybe.map (convertToPokedex pokemon)
                                    |> Result.fromMaybe "could not look up in pokedex"
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
    in
    [ h2 [] [ text "My Team" ]
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
                , viewAttack1 model.effectiveness tp
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
-- RHS My Pokemon
-- -------------------


viewOpponents : Model -> List (Html Msg)
viewOpponents model =
    let
        chooser =
            case model.chooser of
                OpponentChooser search state ->
                    viewChooser model.pokedex search state

                _ ->
                    viewChooserPlaceholder <| OpponentChooser "" Autocomplete.empty

        viewOpponent name entry =
            div [ class "flex flex-row align-items justify-between" ]
                [ viewNameAndTypes name entry
                , deleteIcon <| RemoveOpponent name
                ]

        viewer name =
            case Dict.get name model.pokedex of
                Just entry ->
                    div [ class <| cardClass ++ " bg-white" ] (viewOpponent name entry :: viewPokemonResistsAndWeaknesses model name)

                Nothing ->
                    text <| "Could not look up " ++ name
    in
    [ h2 [] [ text "Opponents" ]
    , chooser
    , (model.opponents ++ Array.toList (Array.map .name model.myPokemon))
        |> L.map viewer
        |> div []
    ]


viewOpponentsBattling : Model -> List (Html Msg)
viewOpponentsBattling model =
    let
        team : List ( String, PType )
        team =
            summariseTeam model

        viewOpponent name entry =
            let
                ( weak, resists ) =
                    checkAttackAgainstDefenderType model.effectiveness team entry.types

                viewLst cls lst =
                    if L.isEmpty lst then
                        text ""

                    else
                        div [ class <| "flex-grow ml-4 p-2 border-solid border-2 " ++ cls ] <|
                            L.map (\( title, pType ) -> colouredBadge pType title) lst
            in
            div [ class "w-1/3 p-2" ]
                [ div [ class <| cardClass ++ " bg-white relative mr-2" ]
                    [ viewNameTitle name
                    , div [ class "recommendations flex flex-col" ]
                        [ viewLst "border-green-500" weak
                        , viewLst "border-red-500" resists
                        ]
                    ]
                ]

        viewer name =
            case Dict.get name model.pokedex of
                Just entry ->
                    viewOpponent name entry

                Nothing ->
                    text <| "Could not look up " ++ name
    in
    [ h2 [] [ text "Opponents" ]
    , (model.opponents ++ Array.toList (Array.map .name model.myPokemon))
        |> L.sort
        |> L.map viewer
        |> div [ class "flex flex-row flex-wrap" ]
    ]


checkAttackAgainstDefenderType : Effectiveness -> List ( String, PType ) -> List PType -> ( List ( String, PType ), List ( String, PType ) )
checkAttackAgainstDefenderType effectiveness team defenderTypes =
    let
        go : ( String, PType ) -> ( List ( String, PType ), List ( String, PType ) ) -> ( List ( String, PType ), List ( String, PType ) )
        go (( _, attackTp ) as val) ( accWeak, accResists ) =
            let
                score =
                    calculateEffectiveness effectiveness attackTp defenderTypes
            in
            if score > 1.1 then
                ( val :: accWeak, accResists )

            else if score < 0.9 then
                ( accWeak, val :: accResists )

            else
                ( accWeak, accResists )
    in
    L.foldl go ( [], [] ) team


summariseTeam model =
    let
        lookup : String -> Maybe Pokemon
        lookup name =
            model.myPokemon
                |> Array.filter (\item -> item.name == name)
                |> Array.get 0
    in
    [ model.team.cand1
    , model.team.cand2
    , model.team.cand3
    ]
        |> L.filterMap
            (\mbName ->
                mbName
                    |> Maybe.andThen lookup
            )
        |> summariseTeamInner model.attacks


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


calculateEffectiveness : Effectiveness -> PType -> List PType -> Float
calculateEffectiveness effectiveness attackTp defenderTypes =
    let
        matrix =
            Dict.get attackTp effectiveness |> Maybe.withDefault Dict.empty

        go : PType -> Float -> Float
        go dType acc =
            matrix
                |> Dict.get dType
                |> Maybe.withDefault 1
                |> (*) acc
    in
    L.foldl go 1 defenderTypes


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
        weaknesses =
            model.pokedex
                |> Dict.get name
                |> Maybe.map .types
                |> Maybe.withDefault []
                |> getDefenceMeta model.effectiveness
    in
    [ viewTypes (\_ f -> f > 1.1) weaknesses "Weak to"
    , viewTypes (\_ f -> f < 0.9) weaknesses "Resists"
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

        --, weaknesses
        --    |> Dict.filter fn
        --    |> Dict.toList
        --    |> L.sortBy (\( _, v ) -> -1 * abs (1 - v))
        --    |> L.map
        --        (\( tp, v ) ->
        --            if v > 0.5 && v < 2 then
        --                span [ class "mr-1" ] [ ppType tp ]
        --
        --            else
        --                span [ class "super mr-3" ] [ ppType tp ]
        --        )
        --    |> div [ class "badge-list flex flex-row flex-wrap" ]
        , div [ class "badge-list flex flex-row flex-wrap" ]
            [ supers |> L.map (\( tp, _ ) -> span [ class "super mr-3" ] [ ppType tp ]) |> div []
            , normals |> L.map (\( tp, _ ) -> span [ class "mr-1" ] [ ppType tp ]) |> div [ class "flex flex-row flex-wrap" ]
            ]
        ]


viewAttacks model entry =
    div [ class "flex flex-col" ]
        [ entry.fast
            |> L.map (viewAttack model)
            |> div [ class "flex flex-col" ]
        , entry.charged
            |> L.map (viewAttack model)
            |> div [ class "flex flex-col" ]
        ]


viewAttack : Model -> String -> Html msg
viewAttack model attack =
    div [ class "flex flex-row items-center" ]
        [ div [ class "mr-2" ] [ viewAttackBadge model.attacks attack ]
        , model.attacks
            |> Dict.get attack
            |> Maybe.map (.type_ >> viewAttack1 model.effectiveness)
            |> Maybe.withDefault (text attack)
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
    "rounded overflow-hidden shadow-lg mb-3 p-1"


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
