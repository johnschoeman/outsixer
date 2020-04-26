module API exposing (..)

import Env exposing (Env)
import Game exposing (Game)
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Json.Decode as Decode
import Outsixer.InputObject as InputObject
import Outsixer.Mutation as Mutation
import Outsixer.Object as Object
import Outsixer.Object.Game as GameObject
import Outsixer.Object.Game_mutation_response as GameMutation
import Outsixer.Query as Query
import RemoteData exposing (RemoteData)


type alias Games =
    List Game


type alias Response data msg =
    RemoteData (Graphql.Http.Error data) data -> msg


fetchGames : Env -> Response Games msg -> Cmd msg
fetchGames env toMsg =
    makeGraphqlQuery env gamesQuery toMsg


gamesQuery : SelectionSet (List Game) RootQuery
gamesQuery =
    Query.game identity gameSelection


gameSelection : SelectionSet Game Object.Game
gameSelection =
    SelectionSet.succeed Game
        |> with GameObject.name
        |> with GameObject.active



-- createGame : Env -> Response Game msg -> Cmd msg
-- createGame env toMsg =
--     makeGraphqlMutation env mutationInsertGame toMsg
-- mutationInsertGame : SelectionSet Game RootMutation
-- mutationInsertGame =
--     Mutation.insert_game {} { name = "Foo" } gameSelection


createGame : Env -> Response (Maybe MutationResponse) msg -> Cmd msg
createGame env toMsg =
    -- makeGraphqlMutation env mutation (RemoteData.fromResult >> GraphQLResponse >> toMsg)
    -- makeGraphqlMutation env mutation (RemoteData.fromResult >> toMsg)
    let
        name =
            "TEST"
    in
    makeGraphqlMutation env (createGameMutation name) toMsg


createGameMutation : String -> SelectionSet (Maybe MutationResponse) RootMutation
createGameMutation name =
    Mutation.insert_game
        identity
        (insertGameArgs name)
        gameMutationResposneSelection


insertGameArgs : String -> Mutation.InsertGameRequiredArguments
insertGameArgs name =
    Mutation.InsertGameRequiredArguments [ insertGameObjects name ]


insertGameObjects : String -> InputObject.Game_insert_input
insertGameObjects name =
    InputObject.buildGame_insert_input (foo name)


foo : String -> InputObject.Game_insert_inputOptionalFields -> InputObject.Game_insert_inputOptionalFields
foo name args =
    { args | name = Present name }


gameMutationResposneSelection : SelectionSet MutationResponse Object.Game_mutation_response
gameMutationResposneSelection =
    SelectionSet.map MutationResponse GameMutation.affected_rows



-- makeMutation : Env -> SelectionSet (Maybe MutationResponse) RootMutation -> (GraphQLResponse String -> msg) -> Cmd msg
-- makeMutation : Env -> SelectionSet (Maybe MutationResponse) RootMutation -> Response String msg -> Cmd msg


type alias MutationResponse =
    { affected_rows : Int
    }


type GraphQLResponse decodesTo
    = GraphQLResponse (RemoteData (Graphql.Http.Error decodesTo) decodesTo)


showHttpError : Graphql.Http.HttpError -> String
showHttpError error =
    case error of
        Graphql.Http.BadStatus metaData message ->
            message

        Graphql.Http.NetworkError ->
            "Network Error"

        Graphql.Http.BadPayload message ->
            Decode.errorToString message

        _ ->
            "Http Error"


makeGraphqlQuery : Env -> SelectionSet data RootQuery -> Response data msg -> Cmd msg
makeGraphqlQuery env query toMsg =
    query
        |> Graphql.Http.queryRequest env.apiEndpoint
        |> Graphql.Http.withHeader "x-hasura-admin-secret" env.apiKey
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)



-- makeGraphqlMutation : Env -> SelectionSet data RootMutation -> Response -> Cmd msg


makeGraphqlMutation : Env -> SelectionSet data RootMutation -> Response data msg -> Cmd msg
makeGraphqlMutation env query toMsg =
    query
        |> Graphql.Http.mutationRequest env.apiEndpoint
        |> Graphql.Http.withHeader "x-hasura-admin-secret" env.apiKey
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)
