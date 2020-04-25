module Autocomplete exposing
    ( view
    , update
    , State(..), empty, reset, resetToFirstItem, resetToLastItem, KeySelected, MouseSelected, KeyStroke(..)
    , Msg(..), ViewConfig, UpdateConfig, HtmlDetails
    )

{-| This library helps you create an autocomplete menu.
Your data is stored separately; keep it in whatever shape makes the most sense for your application.
An autocomplete has a lot of uses: form input, mentions, search, etc.

I have (hopefully!) given the users of this library a large amount of customizability.

I recommend looking at the [examples](https://github.com/thebritican/elm-autocomplete/tree/master/examples) before diving into the API or source code.


# View

@docs view


# Update

@docs update


# State

@docs State, empty, reset, resetToFirstItem, resetToLastItem, KeySelected, MouseSelected, KeyStroke


# Definitions

@docs Msg, ViewConfig, UpdateConfig, HtmlDetails

-}

import Char exposing (Char)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Json.Decode as Decode exposing (Decoder)


type State
    = State InternalState


type alias InternalState =
    { key : Maybe String
    , mouse : Maybe String
    }


type alias KeySelected =
    Bool


type alias MouseSelected =
    Bool


empty : State
empty =
    State emptyInternal


emptyInternal =
    { key = Nothing, mouse = Nothing }


reset : UpdateConfig msg data -> State -> State
reset { separateSelections } (State { key, mouse }) =
    if separateSelections then
        State { key = Nothing, mouse = mouse }

    else
        empty


resetInternal : UpdateConfig msg data -> InternalState -> InternalState
resetInternal { separateSelections } { key, mouse } =
    if separateSelections then
        { key = Nothing, mouse = mouse }

    else
        emptyInternal


resetToFirstItem : UpdateConfig msg data -> List data -> State -> State
resetToFirstItem config data state =
    resetToFirst config data state


resetToFirst : UpdateConfig msg data -> List data -> State -> State
resetToFirst config data (State state) =
    let
        { toId, separateSelections } =
            config

        setFirstItem datum newInternalState =
            { newInternalState | key = Just <| toId datum }
    in
    case List.head data of
        Nothing ->
            empty

        Just datum ->
            if separateSelections then
                resetInternal config state
                    |> setFirstItem datum
                    |> State

            else
                emptyInternal
                    |> setFirstItem datum
                    |> State


resetToLastItem : UpdateConfig msg data -> List data -> State -> State
resetToLastItem config data state =
    let
        reversedData =
            List.reverse data
    in
    resetToFirst config reversedData state



--


type KeyStroke
    = Up
    | Down
    | Enter
    | Esc
    | Other Int


toDirection : String -> Maybe KeyStroke
toDirection string =
    case string of
        "ArrowUp" ->
            Just Up

        "ArrowDown" ->
            Just Down

        "Enter" ->
            Just Enter

        "Escape" ->
            Just Esc

        _ ->
            string |> String.uncons |> Maybe.map (Tuple.first >> Char.toCode >> Other)



-- UPDATE


type alias UpdateConfig msg data =
    { onKeyDown : KeyStroke -> Maybe String -> Maybe msg
    , onInput : String -> msg
    , onTooLow : Maybe msg
    , onTooHigh : Maybe msg
    , onMouseEnter : String -> Maybe msg
    , onMouseLeave : String -> Maybe msg
    , onMouseClick : String -> Maybe msg
    , toId : data -> String
    , separateSelections : Bool
    }


{-| A message type for the autocomplete to update.
-}
type Msg
    = OnFocus Bool
    | OnInput String
    | KeyDown KeyStroke
    | WentTooLow
    | WentTooHigh
    | MouseEnter String
    | MouseLeave String
    | MouseClick String
    | NoOp


{-| Use this function to update the autocomplete's `InternalState`.
Provide the same data as your view.
The `Int` argument is how many results you would like to show.
-}
update : UpdateConfig msg data -> Msg -> State -> List data -> ( State, Maybe msg )
update config msg (State state) data =
    updateInternal config msg state data
        |> Tuple.mapFirst State


updateInternal : UpdateConfig msg data -> Msg -> InternalState -> List data -> ( InternalState, Maybe msg )
updateInternal config msg state data =
    case msg of
        OnFocus isFocused ->
            --if isFocused && state == emptyInternal then
            --    let
            --        firstElement =
            --            data |> List.head |> Maybe.map config.toId
            --    in
            --    ( { key = firstElement, mouse = firstElement }, Nothing )
            --
            --else
            if not isFocused then
                -- input is blurred
                ( emptyInternal, Just <| config.onInput "" )

            else
                -- focused, when state is not empty
                -- Not clear how this could happen
                ( state, Nothing )

        OnInput search ->
            let
                remainingChoices =
                    data
                        |> List.map config.toId
                        |> doSearch False search

                existingSelectedItemPossible =
                    List.any (Just >> (==) state.key) remainingChoices
            in
            ( if existingSelectedItemPossible then
                -- if the selected key is still in the candidates then leave it
                state

              else
                -- choose a new item from remainingChoices
                { state
                    | key = List.head remainingChoices
                    , mouse = List.head remainingChoices
                }
            , Just <| config.onInput search
            )

        KeyDown keystroke ->
            let
                boundedList =
                    List.map config.toId data

                newKey =
                    navigateWithKey keystroke boundedList state.key
            in
            if List.isEmpty boundedList && keystroke == Enter then
                ( state, config.onKeyDown keystroke state.key )

            else if newKey == state.key && keystroke == Up then
                updateInternal config WentTooHigh state data

            else if newKey == state.key && keystroke == Down then
                updateInternal config WentTooLow state data

            else if config.separateSelections then
                ( { state | key = newKey }
                , config.onKeyDown keystroke newKey
                )

            else
                ( { key = newKey, mouse = newKey }
                , config.onKeyDown keystroke newKey
                )

        WentTooLow ->
            ( state
            , config.onTooLow
            )

        WentTooHigh ->
            ( state
            , config.onTooHigh
            )

        MouseEnter id ->
            ( resetMouseInternalStateWithId config.separateSelections id state
            , config.onMouseEnter id
            )

        MouseLeave id ->
            ( resetMouseInternalStateWithId config.separateSelections id state
            , config.onMouseLeave id
            )

        MouseClick id ->
            ( resetMouseInternalStateWithId config.separateSelections id state
            , config.onMouseClick id
            )

        NoOp ->
            ( state, Nothing )


doSearch isCaseSensitive search cands =
    if isCaseSensitive then
        List.filter (\s -> String.contains search s) cands

    else
        let
            search_ =
                String.toLower search
        in
        List.filter (\s -> String.contains search_ (String.toLower s)) cands


navigateWithKey : KeyStroke -> List String -> Maybe String -> Maybe String
navigateWithKey code ids maybeId =
    case code of
        Up ->
            Maybe.map (getPreviousItemId ids) maybeId

        Down ->
            Maybe.map (getNextItemId ids) maybeId

        _ ->
            maybeId


getPreviousItemId : List String -> String -> String
getPreviousItemId ids selectedId =
    Maybe.withDefault selectedId <| List.foldr (getPrevious selectedId) Nothing ids


getNextItemId : List String -> String -> String
getNextItemId ids selectedId =
    Maybe.withDefault selectedId <| List.foldl (getPrevious selectedId) Nothing ids


getPrevious : String -> String -> Maybe String -> Maybe String
getPrevious currVal iterator acc =
    if iterator == currVal then
        -- loop X : Nothing -> Just currVal
        Just currVal

    else if Maybe.withDefault "" acc == currVal then
        -- loop X + 1: sets value, and will never match a second time
        Just iterator

    else
        acc


resetMouseInternalStateWithId : Bool -> String -> InternalState -> InternalState
resetMouseInternalStateWithId separateSelections id state =
    if separateSelections then
        { key = state.key, mouse = Just id }

    else
        { key = Just id, mouse = Just id }



-- VIEW


type alias ViewConfig data =
    { toId : data -> String
    , ul : List (Attribute Never)
    , li : KeySelected -> MouseSelected -> data -> HtmlDetails Never
    }


type alias HtmlDetails msg =
    { attributes : List (Attribute msg)
    , children : List (Html msg)
    }


{-| Take a list of `data` and turn it into an autocomplete menu.
The `ViewConfig` argument is the configuration for the autocomplete view.
`ViewConfig` describes the HTML we want to show for each item and the list.
The `Int` argument is how many results you would like to show.
The `InternalState` argument describes what is selected via mouse and keyboard.

**Note:** The `InternalState` and `List data` should live in your Model.
The `ViewConfig` for the autocomplete belongs in your view code.
`ViewConfig` should never exist in your model.
Describe any potential autocomplete configurations statically.
This pattern has been inspired by [Elm Sortable Table](http://package.elm-lang.org/packages/evancz/elm-sortable-table/latest).

-}
view : ViewConfig data -> State -> List data -> String -> Html Msg
view config (State state) data search =
    viewInternal config state data search



-- VIEW


viewInternal : ViewConfig data -> InternalState -> List data -> String -> Html Msg
viewInternal config state data search =
    div [ class "elm-autocomplete" ]
        [ Html.input
            [ onFocus (OnFocus True)
            , onBlur (OnFocus False)
            , onInput OnInput
            , preventDefaultOn "keydown" keyDecoder
            , value search
            ]
            []
        , if state == emptyInternal then
            text ""

          else
            viewList config state data
        ]


viewList : ViewConfig data -> InternalState -> List data -> Html Msg
viewList config state data =
    let
        customUlAttr =
            List.map mapNeverToMsg config.ul

        getKeyedItems datum =
            ( config.toId datum, viewItem config state datum )
    in
    Html.Keyed.ul customUlAttr
        (List.map getKeyedItems data)


viewItem : ViewConfig data -> InternalState -> data -> Html Msg
viewItem { toId, li } { key, mouse } data =
    let
        id =
            toId data

        listItemData =
            li (isSelected key) (isSelected mouse) data

        customAttributes =
            List.map mapNeverToMsg listItemData.attributes

        customLiAttr =
            List.append customAttributes
                [ Html.Events.onMouseEnter (MouseEnter id)
                , Html.Events.onMouseLeave (MouseLeave id)
                , preventDefaultOn "mousedown" (Decode.succeed ( NoOp, True ))
                , Html.Events.onClick (MouseClick id)
                ]

        isSelected maybeId =
            case maybeId of
                Just someId ->
                    someId == id

                Nothing ->
                    False
    in
    Html.li customLiAttr
        (List.map (Html.map (\html -> NoOp)) listItemData.children)



-- VIEW HELPERS


keyDecoder : Decoder ( Msg, Bool )
keyDecoder =
    let
        mapper ks =
            case ks of
                Just (Other x) ->
                    Decode.succeed ( KeyDown (Other x), False )

                Just ks_ ->
                    -- want to prevent up/down buttons moving cursor
                    Decode.succeed ( KeyDown ks_, True )

                _ ->
                    Decode.fail "expected a Keystroke"
    in
    Decode.map toDirection (Decode.field "key" Decode.string)
        |> Decode.andThen mapper


mapNeverToMsg : Attribute Never -> Attribute Msg
mapNeverToMsg msg =
    Html.Attributes.map (\_ -> NoOp) msg
