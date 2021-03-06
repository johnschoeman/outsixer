-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Outsixer.Object.Game exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import Outsixer.Enum.Player_select_column
import Outsixer.InputObject
import Outsixer.Interface
import Outsixer.Object
import Outsixer.Scalar
import Outsixer.ScalarCodecs
import Outsixer.Union


active : SelectionSet Bool Outsixer.Object.Game
active =
    Object.selectionForField "Bool" "active" [] Decode.bool


created_at : SelectionSet Outsixer.ScalarCodecs.Timestamptz Outsixer.Object.Game
created_at =
    Object.selectionForField "ScalarCodecs.Timestamptz" "created_at" [] (Outsixer.ScalarCodecs.codecs |> Outsixer.Scalar.unwrapCodecs |> .codecTimestamptz |> .decoder)


id : SelectionSet Int Outsixer.Object.Game
id =
    Object.selectionForField "Int" "id" [] Decode.int


name : SelectionSet String Outsixer.Object.Game
name =
    Object.selectionForField "String" "name" [] Decode.string


type alias PlayersOptionalArguments =
    { distinct_on : OptionalArgument (List Outsixer.Enum.Player_select_column.Player_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List Outsixer.InputObject.Player_order_by)
    , where_ : OptionalArgument Outsixer.InputObject.Player_bool_exp
    }


{-| An array relationship

  - distinct\_on - distinct select on columns
  - limit - limit the number of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
players : (PlayersOptionalArguments -> PlayersOptionalArguments) -> SelectionSet decodesTo Outsixer.Object.Player -> SelectionSet (List decodesTo) Outsixer.Object.Game
players fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "distinct_on" filledInOptionals.distinct_on (Encode.enum Outsixer.Enum.Player_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals.limit Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "order_by" filledInOptionals.order_by (Outsixer.InputObject.encodePlayer_order_by |> Encode.list), Argument.optional "where" filledInOptionals.where_ Outsixer.InputObject.encodePlayer_bool_exp ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "players" optionalArgs object_ (identity >> Decode.list)


type alias PlayersAggregateOptionalArguments =
    { distinct_on : OptionalArgument (List Outsixer.Enum.Player_select_column.Player_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List Outsixer.InputObject.Player_order_by)
    , where_ : OptionalArgument Outsixer.InputObject.Player_bool_exp
    }


{-| An aggregated array relationship

  - distinct\_on - distinct select on columns
  - limit - limit the number of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
players_aggregate : (PlayersAggregateOptionalArguments -> PlayersAggregateOptionalArguments) -> SelectionSet decodesTo Outsixer.Object.Player_aggregate -> SelectionSet decodesTo Outsixer.Object.Game
players_aggregate fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "distinct_on" filledInOptionals.distinct_on (Encode.enum Outsixer.Enum.Player_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals.limit Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "order_by" filledInOptionals.order_by (Outsixer.InputObject.encodePlayer_order_by |> Encode.list), Argument.optional "where" filledInOptionals.where_ Outsixer.InputObject.encodePlayer_bool_exp ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "players_aggregate" optionalArgs object_ identity


updated_at : SelectionSet Outsixer.ScalarCodecs.Timestamptz Outsixer.Object.Game
updated_at =
    Object.selectionForField "ScalarCodecs.Timestamptz" "updated_at" [] (Outsixer.ScalarCodecs.codecs |> Outsixer.Scalar.unwrapCodecs |> .codecTimestamptz |> .decoder)


word : SelectionSet String Outsixer.Object.Game
word =
    Object.selectionForField "String" "word" [] Decode.string
