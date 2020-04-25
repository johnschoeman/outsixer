module API exposing (..)

import Api.Object as Object
import Api.Object.Game as GameObject
import Api.Query as Query
import Env exposing (Env)
import Game exposing (Game)
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Json.Decode as Decode
import RemoteData exposing (RemoteData)


type alias Games =
    List Game


fetchGames : Env -> (RemoteData (Graphql.Http.Error Games) Games -> msg) -> Cmd msg
fetchGames env toMsg =
    queryGames
        |> Graphql.Http.queryRequest env.apiEndpoint
        |> Graphql.Http.withHeader "x-hasura-admin-secret" env.apiKey
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


queryGames : SelectionSet (List Game) RootQuery
queryGames =
    Query.game (\a -> a) gameSelection


gameSelection : SelectionSet Game Object.Game
gameSelection =
    SelectionSet.succeed Game
        |> with GameObject.name
        |> with GameObject.active


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
