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
import Outsixer.Object.Player as PlayerObject
import Outsixer.Object.Player_mutation_response as PlayerMutation
import Outsixer.Query as Query
import RemoteData exposing (RemoteData)


type alias Games =
    List Game


type alias Response data msg =
    RemoteData (Graphql.Http.Error data) data -> msg


type GraphQLResponse decodesTo
    = GraphQLResponse (RemoteData (Graphql.Http.Error decodesTo) decodesTo)


type alias MutationResponse =
    { affected_rows : Int
    }


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


makeGraphqlMutation : Env -> SelectionSet data RootMutation -> Response data msg -> Cmd msg
makeGraphqlMutation env query toMsg =
    query
        |> Graphql.Http.mutationRequest env.apiEndpoint
        |> Graphql.Http.withHeader "x-hasura-admin-secret" env.apiKey
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)



---- FETCH GAMES ----


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



---- CREATE GAME ----


createGame : Env -> String -> Response (Maybe MutationResponse) msg -> Cmd msg
createGame env name toMsg =
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
    InputObject.buildGame_insert_input (\args -> { args | name = Present name })


gameMutationResposneSelection : SelectionSet MutationResponse Object.Game_mutation_response
gameMutationResposneSelection =
    SelectionSet.map MutationResponse GameMutation.affected_rows



---- JOIN GAME ----


joinGame : Env -> String -> String -> Response (Maybe MutationResponse) msg -> Cmd msg
joinGame env playerName gameName toMsg =
    makeGraphqlMutation env (joinGameMutation playerName gameName) toMsg


joinGameMutation : String -> String -> SelectionSet (Maybe MutationResponse) RootMutation
joinGameMutation playerName gameName =
    Mutation.insert_player
        identity
        (joinGameArgs playerName gameName)
        gameMutationResposneSelection


joinGameArgs : String -> String -> Mutation.InsertPlayerRequiredArguments
joinGameArgs playerName gameName =
    Mutation.InsertPlayerRequiredArguments [ insertPlayerObjects playerName gameName ]


insertPlayerObjects : String -> String -> InputObject.Player_insert_input
insertPlayerObjects playerName gameId =
    InputObject.buildPlayer_insert_input
        (\args ->
            { args
                | name = Present playerName
                , game_id = Present gameId
            }
        )


playerMutationResposneSelection : SelectionSet MutationResponse Object.Player_mutation_response
playerMutationResposneSelection =
    SelectionSet.map MutationResponse PlayerMutation.affected_rows



---- START GAME ----
