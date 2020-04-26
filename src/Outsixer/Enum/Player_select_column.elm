-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Outsixer.Enum.Player_select_column exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| select columns of table "player"

  - Created\_at - column name
  - Game\_id - column name
  - Id - column name
  - Name - column name
  - Role - column name
  - Updated\_at - column name

-}
type Player_select_column
    = Created_at
    | Game_id
    | Id
    | Name
    | Role
    | Updated_at


list : List Player_select_column
list =
    [ Created_at, Game_id, Id, Name, Role, Updated_at ]


decoder : Decoder Player_select_column
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "created_at" ->
                        Decode.succeed Created_at

                    "game_id" ->
                        Decode.succeed Game_id

                    "id" ->
                        Decode.succeed Id

                    "name" ->
                        Decode.succeed Name

                    "role" ->
                        Decode.succeed Role

                    "updated_at" ->
                        Decode.succeed Updated_at

                    _ ->
                        Decode.fail ("Invalid Player_select_column type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : Player_select_column -> String
toString enum =
    case enum of
        Created_at ->
            "created_at"

        Game_id ->
            "game_id"

        Id ->
            "id"

        Name ->
            "name"

        Role ->
            "role"

        Updated_at ->
            "updated_at"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe Player_select_column
fromString enumString =
    case enumString of
        "created_at" ->
            Just Created_at

        "game_id" ->
            Just Game_id

        "id" ->
            Just Id

        "name" ->
            Just Name

        "role" ->
            Just Role

        "updated_at" ->
            Just Updated_at

        _ ->
            Nothing
