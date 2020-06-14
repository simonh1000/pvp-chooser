module Pokemon exposing (..)

import AssocList as Dict exposing (Dict)
import Common.CoreHelpers exposing (exactMatchString)
import Json.Decode as Decode exposing (Decoder, Value)
import List as L



-- -----------------------
-- PType
-- -----------------------


type PType
    = Poison
    | Water
    | Normal
    | Flying
    | Grass
    | Ground
    | Psychic
    | Fire
    | Bug
    | Rock
    | Electric
    | Fighting
    | Fairy
    | Ice
    | Ghost
    | Dragon
    | Steel
    | Dark


{-| We want to drop "none"s
-}
decodeTypes : Decoder (List PType)
decodeTypes =
    Decode.maybe decodePType
        |> Decode.list
        |> Decode.map (L.filterMap identity)


decodePType : Decoder PType
decodePType =
    let
        matcher tgt v =
            exactMatchString Decode.string tgt (Decode.succeed v)
    in
    Decode.oneOf
        [ matcher "poison" Poison
        , matcher "water" Water
        , matcher "normal" Normal
        , matcher "flying" Flying
        , matcher "grass" Grass
        , matcher "ground" Ground
        , matcher "psychic" Psychic
        , matcher "fire" Fire
        , matcher "bug" Bug
        , matcher "rock" Rock
        , matcher "electric" Electric
        , matcher "fighting" Fighting
        , matcher "fairy" Fairy
        , matcher "ice" Ice
        , matcher "ghost" Ghost
        , matcher "dragon" Dragon
        , matcher "steel" Steel
        , matcher "dark" Dark
        ]


stringFromPType : PType -> ( String, String )
stringFromPType tp =
    case tp of
        Poison ->
            ( "Poison", "#a040a0" )

        Water ->
            ( "Water", "#8ebef9" )

        Normal ->
            ( "Normal", "#989299" )

        Flying ->
            ( "Flying", "#aebaf0" )

        Grass ->
            ( "Grass", "#78c850" )

        Ground ->
            ( "Ground", "#cc8038" )

        Psychic ->
            ( "Psychic", "#f85888" )

        Fire ->
            ( "Fire", "#f08030" )

        Bug ->
            ( "Bug", "#9bb824" )

        Rock ->
            ( "Rock", "#b8a743" )

        Electric ->
            ( "Electric", "#f8d030" )

        Fighting ->
            ( "Fighting", "#c03028" )

        Fairy ->
            ( "Fairy", "#e898e8" )

        Ice ->
            ( "Ice", "#98d8d8" )

        Ghost ->
            ( "Ghost", "#705898" )

        Dragon ->
            ( "Dragon", "#4368b9" )

        Steel ->
            ( "Steel", "#799ea2" )

        Dark ->
            ( "Dark", "#707070" )



-- -----------------------
-- Effectiveness
-- -----------------------


type alias Effectiveness =
    Dict PType (Dict PType Float)


effectiveness : Effectiveness
effectiveness =
    Dict.fromList
        [ ( Water, Dict.fromList [ ( Water, 0.625 ), ( Steel, 1 ), ( Rock, 1.6 ), ( Psychic, 1 ), ( Poison, 1 ), ( Normal, 1 ), ( Ice, 1 ), ( Ground, 1.6 ), ( Grass, 0.625 ), ( Ghost, 1 ), ( Flying, 1 ), ( Fire, 1.6 ), ( Fighting, 1 ), ( Fairy, 1 ), ( Electric, 1 ), ( Dragon, 0.625 ), ( Dark, 1 ), ( Bug, 1 ) ] )
        , ( Steel, Dict.fromList [ ( Water, 0.625 ), ( Steel, 0.625 ), ( Rock, 1.6 ), ( Psychic, 1 ), ( Poison, 1 ), ( Normal, 1 ), ( Ice, 1.6 ), ( Ground, 1 ), ( Grass, 1 ), ( Ghost, 1 ), ( Flying, 1 ), ( Fire, 0.625 ), ( Fighting, 1 ), ( Fairy, 1.6 ), ( Electric, 0.625 ), ( Dragon, 1 ), ( Dark, 1 ), ( Bug, 1 ) ] )
        , ( Rock, Dict.fromList [ ( Water, 1 ), ( Steel, 0.625 ), ( Rock, 1 ), ( Psychic, 1 ), ( Poison, 1 ), ( Normal, 1 ), ( Ice, 1.6 ), ( Ground, 0.625 ), ( Grass, 1 ), ( Ghost, 1 ), ( Flying, 1.6 ), ( Fire, 1.6 ), ( Fighting, 0.625 ), ( Fairy, 1 ), ( Electric, 1 ), ( Dragon, 1 ), ( Dark, 1 ), ( Bug, 1.6 ) ] )
        , ( Psychic, Dict.fromList [ ( Water, 1 ), ( Steel, 0.625 ), ( Rock, 1 ), ( Psychic, 0.625 ), ( Poison, 1.6 ), ( Normal, 1 ), ( Ice, 1 ), ( Ground, 1 ), ( Grass, 1 ), ( Ghost, 1 ), ( Flying, 1 ), ( Fire, 1 ), ( Fighting, 1.6 ), ( Fairy, 1 ), ( Electric, 1 ), ( Dragon, 1 ), ( Dark, 0.390625 ), ( Bug, 1 ) ] )
        , ( Poison, Dict.fromList [ ( Water, 1 ), ( Steel, 0.390625 ), ( Rock, 0.625 ), ( Psychic, 1 ), ( Poison, 0.625 ), ( Normal, 1 ), ( Ice, 1 ), ( Ground, 0.625 ), ( Grass, 1.6 ), ( Ghost, 0.625 ), ( Flying, 1 ), ( Fire, 1 ), ( Fighting, 1 ), ( Fairy, 1.6 ), ( Electric, 1 ), ( Dragon, 1 ), ( Dark, 1 ), ( Bug, 1 ) ] )
        , ( Normal, Dict.fromList [ ( Water, 1 ), ( Steel, 0.625 ), ( Rock, 0.625 ), ( Psychic, 1 ), ( Poison, 1 ), ( Normal, 1 ), ( Ice, 1 ), ( Ground, 1 ), ( Grass, 1 ), ( Ghost, 0.390625 ), ( Flying, 1 ), ( Fire, 1 ), ( Fighting, 1 ), ( Fairy, 1 ), ( Electric, 1 ), ( Dragon, 1 ), ( Dark, 1 ), ( Bug, 1 ) ] )
        , ( Ice, Dict.fromList [ ( Water, 0.625 ), ( Steel, 0.625 ), ( Rock, 1 ), ( Psychic, 1 ), ( Poison, 1 ), ( Normal, 1 ), ( Ice, 0.625 ), ( Ground, 1.6 ), ( Grass, 1.6 ), ( Ghost, 1 ), ( Flying, 1.6 ), ( Fire, 0.625 ), ( Fighting, 1 ), ( Fairy, 1 ), ( Electric, 1 ), ( Dragon, 1.6 ), ( Dark, 1 ), ( Bug, 1 ) ] )
        , ( Ground, Dict.fromList [ ( Water, 1 ), ( Steel, 1.6 ), ( Rock, 1.6 ), ( Psychic, 1 ), ( Poison, 1.6 ), ( Normal, 1 ), ( Ice, 1 ), ( Ground, 1 ), ( Grass, 0.625 ), ( Ghost, 1 ), ( Flying, 0.390625 ), ( Fire, 1.6 ), ( Fighting, 1 ), ( Fairy, 1 ), ( Electric, 1.6 ), ( Dragon, 1 ), ( Dark, 1 ), ( Bug, 0.625 ) ] )
        , ( Grass, Dict.fromList [ ( Water, 1.6 ), ( Steel, 0.625 ), ( Rock, 1.6 ), ( Psychic, 1 ), ( Poison, 0.625 ), ( Normal, 1 ), ( Ice, 1 ), ( Ground, 1.6 ), ( Grass, 0.625 ), ( Ghost, 1 ), ( Flying, 0.625 ), ( Fire, 0.625 ), ( Fighting, 1 ), ( Fairy, 1 ), ( Electric, 1 ), ( Dragon, 0.625 ), ( Dark, 1 ), ( Bug, 0.625 ) ] )
        , ( Ghost, Dict.fromList [ ( Water, 1 ), ( Steel, 1 ), ( Rock, 1 ), ( Psychic, 1.6 ), ( Poison, 1 ), ( Normal, 0.390625 ), ( Ice, 1 ), ( Ground, 1 ), ( Grass, 1 ), ( Ghost, 1.6 ), ( Flying, 1 ), ( Fire, 1 ), ( Fighting, 1 ), ( Fairy, 1 ), ( Electric, 1 ), ( Dragon, 1 ), ( Dark, 0.625 ), ( Bug, 1 ) ] )
        , ( Flying, Dict.fromList [ ( Water, 1 ), ( Steel, 0.625 ), ( Rock, 0.625 ), ( Psychic, 1 ), ( Poison, 1 ), ( Normal, 1 ), ( Ice, 1 ), ( Ground, 1 ), ( Grass, 1.6 ), ( Ghost, 1 ), ( Flying, 1 ), ( Fire, 1 ), ( Fighting, 1.6 ), ( Fairy, 1 ), ( Electric, 0.625 ), ( Dragon, 1 ), ( Dark, 1 ), ( Bug, 1.6 ) ] )
        , ( Fire, Dict.fromList [ ( Water, 0.625 ), ( Steel, 1.6 ), ( Rock, 0.625 ), ( Psychic, 1 ), ( Poison, 1 ), ( Normal, 1 ), ( Ice, 1.6 ), ( Ground, 1 ), ( Grass, 1.6 ), ( Ghost, 1 ), ( Flying, 1 ), ( Fire, 0.625 ), ( Fighting, 1 ), ( Fairy, 1 ), ( Electric, 1 ), ( Dragon, 0.625 ), ( Dark, 1 ), ( Bug, 1.6 ) ] )
        , ( Fighting, Dict.fromList [ ( Water, 1 ), ( Steel, 1.6 ), ( Rock, 1.6 ), ( Psychic, 0.625 ), ( Poison, 0.625 ), ( Normal, 1.6 ), ( Ice, 1.6 ), ( Ground, 1 ), ( Grass, 1 ), ( Ghost, 0.390625 ), ( Flying, 0.625 ), ( Fire, 1 ), ( Fighting, 1 ), ( Fairy, 0.625 ), ( Electric, 1 ), ( Dragon, 1 ), ( Dark, 1.6 ), ( Bug, 0.625 ) ] )
        , ( Fairy, Dict.fromList [ ( Water, 1 ), ( Steel, 0.625 ), ( Rock, 1 ), ( Psychic, 1 ), ( Poison, 0.625 ), ( Normal, 1 ), ( Ice, 1 ), ( Ground, 1 ), ( Grass, 1 ), ( Ghost, 1 ), ( Flying, 1 ), ( Fire, 0.625 ), ( Fighting, 1.6 ), ( Fairy, 1 ), ( Electric, 1 ), ( Dragon, 1.6 ), ( Dark, 1.6 ), ( Bug, 1 ) ] )
        , ( Electric, Dict.fromList [ ( Water, 1.6 ), ( Steel, 1 ), ( Rock, 1 ), ( Psychic, 1 ), ( Poison, 1 ), ( Normal, 1 ), ( Ice, 1 ), ( Ground, 0.390625 ), ( Grass, 0.625 ), ( Ghost, 1 ), ( Flying, 1.6 ), ( Fire, 1 ), ( Fighting, 1 ), ( Fairy, 1 ), ( Electric, 0.625 ), ( Dragon, 0.625 ), ( Dark, 1 ), ( Bug, 1 ) ] )
        , ( Dragon, Dict.fromList [ ( Water, 1 ), ( Steel, 0.625 ), ( Rock, 1 ), ( Psychic, 1 ), ( Poison, 1 ), ( Normal, 1 ), ( Ice, 1 ), ( Ground, 1 ), ( Grass, 1 ), ( Ghost, 1 ), ( Flying, 1 ), ( Fire, 1 ), ( Fighting, 1 ), ( Fairy, 0.390625 ), ( Electric, 1 ), ( Dragon, 1.6 ), ( Dark, 1 ), ( Bug, 1 ) ] )
        , ( Dark, Dict.fromList [ ( Water, 1 ), ( Steel, 1 ), ( Rock, 1 ), ( Psychic, 1.6 ), ( Poison, 1 ), ( Normal, 1 ), ( Ice, 1 ), ( Ground, 1 ), ( Grass, 1 ), ( Ghost, 1.6 ), ( Flying, 1 ), ( Fire, 1 ), ( Fighting, 0.625 ), ( Fairy, 0.625 ), ( Electric, 1 ), ( Dragon, 1 ), ( Dark, 0.625 ), ( Bug, 1 ) ] )
        , ( Bug, Dict.fromList [ ( Water, 1 ), ( Steel, 0.625 ), ( Rock, 1 ), ( Psychic, 1.6 ), ( Poison, 0.625 ), ( Normal, 1 ), ( Ice, 1 ), ( Ground, 1 ), ( Grass, 1.6 ), ( Ghost, 0.625 ), ( Flying, 0.625 ), ( Fire, 0.625 ), ( Fighting, 0.625 ), ( Fairy, 0.625 ), ( Electric, 1 ), ( Dragon, 1 ), ( Dark, 1.6 ), ( Bug, 1 ) ] )
        ]
