-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Outsixer.Object.Player_var_samp_fields exposing (..)

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


game_id : SelectionSet (Maybe Float) Outsixer.Object.Player_var_samp_fields
game_id =
    Object.selectionForField "(Maybe Float)" "game_id" [] (Decode.float |> Decode.nullable)


id : SelectionSet (Maybe Float) Outsixer.Object.Player_var_samp_fields
id =
    Object.selectionForField "(Maybe Float)" "id" [] (Decode.float |> Decode.nullable)
