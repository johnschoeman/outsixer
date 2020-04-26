-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Outsixer.Object.Player_aggregate exposing (..)

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


aggregate : SelectionSet decodesTo Outsixer.Object.Player_aggregate_fields -> SelectionSet (Maybe decodesTo) Outsixer.Object.Player_aggregate
aggregate object_ =
    Object.selectionForCompositeField "aggregate" [] object_ (identity >> Decode.nullable)


nodes : SelectionSet decodesTo Outsixer.Object.Player -> SelectionSet (List decodesTo) Outsixer.Object.Player_aggregate
nodes object_ =
    Object.selectionForCompositeField "nodes" [] object_ (identity >> Decode.list)
