-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Outsixer.Object.Player_min_fields exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import Outsixer.InputObject
import Outsixer.Interface
import Outsixer.Object
import Outsixer.Scalar
import Outsixer.ScalarCodecs
import Outsixer.Union


created_at : SelectionSet (Maybe Outsixer.ScalarCodecs.Timestamptz) Outsixer.Object.Player_min_fields
created_at =
    Object.selectionForField "(Maybe ScalarCodecs.Timestamptz)" "created_at" [] (Outsixer.ScalarCodecs.codecs |> Outsixer.Scalar.unwrapCodecs |> .codecTimestamptz |> .decoder |> Decode.nullable)


game_id : SelectionSet (Maybe Int) Outsixer.Object.Player_min_fields
game_id =
    Object.selectionForField "(Maybe Int)" "game_id" [] (Decode.int |> Decode.nullable)


id : SelectionSet (Maybe Int) Outsixer.Object.Player_min_fields
id =
    Object.selectionForField "(Maybe Int)" "id" [] (Decode.int |> Decode.nullable)


name : SelectionSet (Maybe String) Outsixer.Object.Player_min_fields
name =
    Object.selectionForField "(Maybe String)" "name" [] (Decode.string |> Decode.nullable)


updated_at : SelectionSet (Maybe Outsixer.ScalarCodecs.Timestamptz) Outsixer.Object.Player_min_fields
updated_at =
    Object.selectionForField "(Maybe ScalarCodecs.Timestamptz)" "updated_at" [] (Outsixer.ScalarCodecs.codecs |> Outsixer.Scalar.unwrapCodecs |> .codecTimestamptz |> .decoder |> Decode.nullable)
