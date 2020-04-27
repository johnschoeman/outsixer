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
import Player exposing (Player)
import RemoteData exposing (RemoteData)


type alias Games =
    List Game


type alias Response data msg =
    RemoteData (Graphql.Http.Error data) data -> msg


type GraphQLResponse decodesTo
    = GraphQLResponse (RemoteData (Graphql.Http.Error decodesTo) decodesTo)


type alias AffectedRowsResponse =
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



---- FETCH PLAYERS ----


type alias PlayersResponse =
    RemoteData (Graphql.Http.Error (List Player)) (List Player)


fetchPlayers : Env -> Int -> Response (List Player) msg -> Cmd msg
fetchPlayers env gameId toMsg =
    makeGraphqlQuery env playersQuery toMsg


playersQuery : SelectionSet (List Player) RootQuery
playersQuery =
    Query.player identity playerSelection


playerSelection : SelectionSet Player Object.Player
playerSelection =
    SelectionSet.succeed Player
        |> with PlayerObject.id
        |> with PlayerObject.name
        |> with PlayerObject.game_id
        |> with PlayerObject.role



---- FETCH GAME ----
-- query {
--   game(where: {id: {_eq: 1}}) {
--     active
--     word
--     players {
--       id
--       role
--       name
--     }
--   }
-- }


type alias GameResponseData =
    List GameData


type alias GameData =
    { id : Int
    , name : String
    , word : String
    , active : Bool
    , players : List Player
    }


type alias GameResponse =
    RemoteData (Graphql.Http.Error GameResponseData) GameResponseData


fetchGame : Env -> Int -> Response GameResponseData msg -> Cmd msg
fetchGame env gameId toMsg =
    makeGraphqlQuery env (gameQuery gameId) toMsg


gameQuery : Int -> SelectionSet GameResponseData RootQuery
gameQuery gameId =
    Query.game (gameOptionalArgument gameId) gameSelection


gameOptionalArgument : Int -> Query.GameOptionalArguments -> Query.GameOptionalArguments
gameOptionalArgument gameId optionalArgs =
    { optionalArgs | where_ = whereGameIdIsEq gameId }


whereGameIdIsEq : Int -> OptionalArgument InputObject.Game_bool_exp
whereGameIdIsEq gameId =
    Present <| InputObject.buildGame_bool_exp (\args -> { args | id = idIsEq gameId })


idIsEq : Int -> OptionalArgument InputObject.Int_comparison_exp
idIsEq gameId =
    Present <| InputObject.buildInt_comparison_exp (\args -> { args | eq_ = Present gameId })


gameSelection : SelectionSet GameData Object.Game
gameSelection =
    SelectionSet.map5 GameData
        GameObject.id
        GameObject.name
        GameObject.word
        GameObject.active
        (GameObject.players identity playerFragment)



---- CREATE GAME ----
-- mutation ($name: String!) {
--   insert_game(objects: {name: $name}) {
--     affected_rows
--     returning {
--       id
--       name
--       active
--     }
--   }
-- }


type alias CreateGameResponseData =
    { returning : List CreateGameData
    }


type alias CreateGameData =
    { id : Int
    , name : String
    , active : Bool
    }


type alias CreateGameResponse =
    RemoteData (Graphql.Http.Error (Maybe CreateGameResponseData)) (Maybe CreateGameResponseData)


createGame : Env -> String -> Response (Maybe CreateGameResponseData) msg -> Cmd msg
createGame env name toMsg =
    makeGraphqlMutation env (createGameMutation name) toMsg


createGameMutation : String -> SelectionSet (Maybe CreateGameResponseData) RootMutation
createGameMutation name =
    Mutation.insert_game
        identity
        (insertGameArgs name)
        createGameMutationResponse


insertGameArgs : String -> Mutation.InsertGameRequiredArguments
insertGameArgs name =
    Mutation.InsertGameRequiredArguments [ insertGameObjects name ]


insertGameObjects : String -> InputObject.Game_insert_input
insertGameObjects name =
    InputObject.buildGame_insert_input (\args -> { args | name = Present name })


createGameMutationResponse : SelectionSet CreateGameResponseData Object.Game_mutation_response
createGameMutationResponse =
    SelectionSet.map CreateGameResponseData
        (GameMutation.returning gameFragment)


gameFragment : SelectionSet CreateGameData Object.Game
gameFragment =
    SelectionSet.succeed CreateGameData
        |> with GameObject.id
        |> with GameObject.name
        |> with GameObject.active



---- JOIN LOBBY ----
-- mutation ($name: String! $game_id: Int!) {
--   insert_game(objects: {name: $name, game_id: $game_id}) {
--     returning {
--       id
--       name
--       game_id
--       role
--     }
--   }
-- }


type alias JoinLobbyResponseData =
    { returning : List Player
    }


type alias JoinLobbyResponse =
    RemoteData (Graphql.Http.Error (Maybe JoinLobbyResponseData)) (Maybe JoinLobbyResponseData)


joinLobby : Env -> String -> Int -> Response (Maybe JoinLobbyResponseData) msg -> Cmd msg
joinLobby env playerName gameId toMsg =
    makeGraphqlMutation env (joinLobbyMutation playerName gameId) toMsg


joinLobbyMutation : String -> Int -> SelectionSet (Maybe JoinLobbyResponseData) RootMutation
joinLobbyMutation playerName gameId =
    Mutation.insert_player
        identity
        (joinGameArgs playerName gameId)
        playerAffectedRowsResponseSelection


joinGameArgs : String -> Int -> Mutation.InsertPlayerRequiredArguments
joinGameArgs playerName gameId =
    Mutation.InsertPlayerRequiredArguments [ insertPlayerObjects playerName gameId ]


insertPlayerObjects : String -> Int -> InputObject.Player_insert_input
insertPlayerObjects playerName gameId =
    InputObject.buildPlayer_insert_input
        (\args ->
            { args
                | name = Present playerName
                , game_id = Present gameId
            }
        )


playerAffectedRowsResponseSelection : SelectionSet JoinLobbyResponseData Object.Player_mutation_response
playerAffectedRowsResponseSelection =
    SelectionSet.map JoinLobbyResponseData returningPlayerFragment


returningPlayerFragment : SelectionSet (List Player) Object.Player_mutation_response
returningPlayerFragment =
    PlayerMutation.returning playerFragment


playerFragment : SelectionSet Player Object.Player
playerFragment =
    SelectionSet.succeed Player
        |> with PlayerObject.id
        |> with PlayerObject.name
        |> with PlayerObject.game_id
        |> with PlayerObject.role



---- START GAME ----
-- mutation($id: !Int $word: !String) {
--   update_game(_set: {word: $word, active: true}, _inc: {id: $id})


type alias StartGameResponseData =
    ()


type alias StartGameResponse =
    RemoteData (Graphql.Http.Error (Maybe StartGameResponseData)) (Maybe StartGameResponseData)


startGame : Env -> Int -> String -> Response (Maybe StartGameResponseData) msg -> Cmd msg
startGame env gameId word toMsg =
    makeGraphqlMutation env (startGameMutation gameId word) toMsg


startGameMutation : Int -> String -> SelectionSet (Maybe StartGameResponseData) RootMutation
startGameMutation gameId word =
    Mutation.update_game
        (startGameMutationOptionalArgs word)
        (startGameWhereArgs gameId)
        startGameMutationResponse


startGameMutationOptionalArgs : String -> Mutation.UpdateGameOptionalArguments -> Mutation.UpdateGameOptionalArguments
startGameMutationOptionalArgs word optionalArgs =
    { optionalArgs | set_ = Present (startGameSetInputArgs word) }


startGameSetInputArgs : String -> InputObject.Game_set_input
startGameSetInputArgs word =
    InputObject.buildGame_set_input (\args -> { args | word = Present word, active = Present True })


startGameWhereArgs : Int -> Mutation.UpdateGameRequiredArguments
startGameWhereArgs gameId =
    Mutation.UpdateGameRequiredArguments
        (InputObject.buildGame_bool_exp (\args -> { args | id = idIsEq gameId }))


startGameMutationResponse : SelectionSet () Object.Game_mutation_response
startGameMutationResponse =
    SelectionSet.succeed ()
