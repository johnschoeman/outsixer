-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.Games exposing (..)

import Api.InputObject
import Api.Interface
import Api.Object
import Api.Scalar
import Api.ScalarCodecs
import Api.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


active : SelectionSet Bool Api.Object.Games
active =
    Object.selectionForField "Bool" "active" [] Decode.bool


created_at : SelectionSet Api.ScalarCodecs.Timestamptz Api.Object.Games
created_at =
    Object.selectionForField "ScalarCodecs.Timestamptz" "created_at" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecTimestamptz |> .decoder)


id : SelectionSet Int Api.Object.Games
id =
    Object.selectionForField "Int" "id" [] Decode.int


name : SelectionSet String Api.Object.Games
name =
    Object.selectionForField "String" "name" [] Decode.string


updated_at : SelectionSet Api.ScalarCodecs.Timestamptz Api.Object.Games
updated_at =
    Object.selectionForField "ScalarCodecs.Timestamptz" "updated_at" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecTimestamptz |> .decoder)
