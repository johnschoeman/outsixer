-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Outsixer.Enum.Player_constraint exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| unique or primary key constraints on table "player"

  - Player\_pkey - unique or primary key constraint

-}
type Player_constraint
    = Player_pkey


list : List Player_constraint
list =
    [ Player_pkey ]


decoder : Decoder Player_constraint
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "player_pkey" ->
                        Decode.succeed Player_pkey

                    _ ->
                        Decode.fail ("Invalid Player_constraint type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : Player_constraint -> String
toString enum =
    case enum of
        Player_pkey ->
            "player_pkey"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe Player_constraint
fromString enumString =
    case enumString of
        "player_pkey" ->
            Just Player_pkey

        _ ->
            Nothing
