module Main exposing (..)

import API exposing (GraphQLResponse)
import Browser
import Env exposing (Env)
import Game exposing (Game)
import Graphql.Http
import Html exposing (Html, button, div, h1, img, input, label, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick, onInput)
import RemoteData exposing (RemoteData)



---- MODEL ----


type Model
    = MainApp AppData Env


type alias AppData =
    { games : GameData
    , playerName : String
    , gameName : String
    }


type alias GraphQLResponse data =
    RemoteData (Graphql.Http.Error data) data


type alias MutationResponse =
    { affected_rows : Int }


type alias GameData =
    GraphQLResponse (List Game)


type alias CreateGameData =
    GraphQLResponse (Maybe MutationResponse)


type alias JoinGameData =
    GraphQLResponse (Maybe MutationResponse)


type alias StartGameData =
    GraphQLResponse (Maybe MutationResponse)


type alias Flags =
    { apiEndpoint : String
    , apiKey : String
    }


init : Flags -> ( Model, Cmd Msg )
init { apiEndpoint, apiKey } =
    let
        env =
            { apiEndpoint = apiEndpoint, apiKey = apiKey }
    in
    ( MainApp
        { games = RemoteData.NotAsked
        , gameName = ""
        , playerName = ""
        }
        env
    , API.fetchGames env ReceivedGames
    )



---- UPDATE ----


type Msg
    = UpdatePlayerName String
    | UpdateGameName String
    | ReceivedGames GameData
    | CreateGame
    | ReceivedCreateGameResponse CreateGameData
    | JoinGame
    | ReceivedJoinGameResponse JoinGameData
    | StartGame
    | ReceivedStartGameResponse StartGameData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( MainApp appData env, UpdatePlayerName playerName ) ->
            ( MainApp { appData | playerName = Just playerName } env, Cmd.none )

        ( MainApp appData env, UpdateGameName gameName ) ->
            ( MainApp { appData | gameName = Just gameName } env, Cmd.none )

        ( MainApp appData env, ReceivedGames gameData ) ->
            ( MainApp { appData | games = gameData } env, Cmd.none )

        ( MainApp _ env, CreateGame ) ->
            let
                newGameName =
                    "GAME"
            in
            ( model, API.createGame env ReceivedCreateGameResponse )

        ( MainApp appData env, ReceivedCreateGameResponse createGameData ) ->
            ( MainApp appData env, Cmd.none )

        ( MainApp { playerName, gameName } env, JoinGame ) ->
            ( model, API.joinGame env playerName gameName ReceivedJoinGameResponse )

        ( _, ReceivedJoinGameResponse joinGameData ) ->
            ( model, Cmd.none )

        ( _, StartGame ) ->
            ( model, Cmd.none )

        ( _, ReceivedStartGameResponse startGameData ) ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Outsixer", body = [ page model ] }


page : Model -> Html Msg
page model =
    case model of
        MainApp appData _ ->
            div []
                [ h1 [] [ text "Outsixer" ]
                , nameInput appData.playerName
                , joinGame appData.gameName
                , createNewGame
                , listGames appData.games
                ]


nameInput : String -> Html Msg
nameInput name =
    div []
        [ label [] [ text "Player Name: " ]
        , input [ onInput UpdatePlayerName ] [ text name ]
        ]


joinGame : String -> Html Msg
joinGame gameName =
    div []
        [ label [] [ text "Game Name: " ]
        , input [ onInput UpdateGameName ] [ text gameName ]
        , button [ onClick JoinGame ] [ text "Join Game" ]
        ]


createNewGame : Html Msg
createNewGame =
    div []
        [ button [ onClick CreateGame ] [ text "CreateGame" ]
        ]


listGames : GameData -> Html msg
listGames gameData =
    case gameData of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            text "loading"

        RemoteData.Failure error ->
            errorMessage error

        RemoteData.Success games ->
            div [] (List.map gameListItem games)


gameListItem : Game -> Html msg
gameListItem game =
    div []
        [ text game.name
        ]


errorMessage : Graphql.Http.Error d -> Html msg
errorMessage error =
    case error of
        Graphql.Http.GraphqlError p graphqlErrors ->
            div [] [ text "graphqlError" ]

        Graphql.Http.HttpError httpError ->
            div [] [ text <| API.showHttpError httpError ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.document
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
