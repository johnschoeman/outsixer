module Main exposing (..)

import API exposing (GraphQLResponse)
import Browser
import Env exposing (Env)
import Game exposing (Game)
import Graphql.Http
import Html exposing (Html, button, div, h1, img, input, label, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick, onInput)
import Player exposing (Player, Role)
import RemoteData exposing (RemoteData)
import Time



---- MODEL ----


type Model
    = PreGame PlayerName GameId Env
    | Lobby LobbyState Env
    | InGame GameState Env


type alias PlayerName =
    String


type alias GameId =
    String


type alias Word =
    String


type alias LobbyState =
    { player : Player
    , players : List Player
    , gameId : Int
    }


type alias GameState =
    { player : Player
    , players : List Player
    , word : String
    }


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
    ( PreGame "" "" env
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Tick Time.Posix
    | UpdatePlayerName String
    | UpdateGameId String
    | CreateGame
    | ReceivedCreateGameResponse API.CreateGameResponse
    | JoinLobby
    | ReceivedJoinLobbyResponse API.JoinLobbyResponse
    | ReceivedGameResponse API.GameResponse
    | StartGame
    | ReceivedStartGameResponse API.StartGameResponse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( PreGame _ gameId env, UpdatePlayerName playerName ) ->
            ( PreGame playerName gameId env, Cmd.none )

        ( PreGame playerName _ env, UpdateGameId gameId ) ->
            ( PreGame playerName gameId env, Cmd.none )

        ( PreGame playerName _ env, CreateGame ) ->
            let
                gameName =
                    "GAME"
            in
            ( model, API.createGame env gameName ReceivedCreateGameResponse )

        ( PreGame playerName _ env, ReceivedCreateGameResponse response ) ->
            case response of
                RemoteData.Success createGameData ->
                    case createGameData of
                        Just gameData ->
                            case List.head gameData.returning of
                                Just game ->
                                    ( model, API.joinLobby env playerName game.id ReceivedJoinLobbyResponse )

                                Nothing ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( PreGame playerName gameId env, JoinLobby ) ->
            case String.toInt gameId of
                Just id ->
                    ( model, API.joinLobby env playerName id ReceivedJoinLobbyResponse )

                Nothing ->
                    ( model, Cmd.none )

        ( PreGame _ _ env, ReceivedJoinLobbyResponse response ) ->
            case response of
                RemoteData.Success joinLobbyData ->
                    case joinLobbyData of
                        Just data ->
                            let
                                maybePlayer =
                                    List.head data.returning
                            in
                            case maybePlayer of
                                Just player ->
                                    let
                                        lobbyData =
                                            { player = player, players = [], gameId = player.gameId }
                                    in
                                    ( Lobby lobbyData env, Cmd.none )

                                Nothing ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( PreGame _ _ _, _ ) ->
            ( model, Cmd.none )

        ( Lobby { gameId } env, Tick _ ) ->
            ( model, API.fetchGame env gameId ReceivedGameResponse )

        ( Lobby lobbyData env, ReceivedGameResponse response ) ->
            case response of
                RemoteData.Success responseData ->
                    case List.head responseData of
                        Just game ->
                            if game.active then
                                let
                                    gameData =
                                        { player = lobbyData.player
                                        , players = Debug.log "players" game.players
                                        , word = game.word
                                        }
                                in
                                ( InGame gameData env, Cmd.none )

                            else
                                ( Lobby { lobbyData | players = game.players } env, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( Lobby lobbyData env, StartGame ) ->
            let
                word =
                    "Word"
            in
            ( model, API.startGame env lobbyData.gameId word ReceivedStartGameResponse )

        ( Lobby _ env, ReceivedStartGameResponse response ) ->
            case response of
                RemoteData.Success startGameData ->
                    let
                        player =
                            { id = 1, name = "name", role = Just "Master" }

                        players =
                            [ player ]

                        gameData =
                            { player = player, players = players, word = "Bunny" }
                    in
                    -- ( InGame gameData env, Cmd.none )
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( Lobby _ _, _ ) ->
            ( model, Cmd.none )

        ( InGame _ _, _ ) ->
            ( model, Cmd.none )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Outsixer", body = [ page model ] }


page : Model -> Html Msg
page model =
    case model of
        PreGame playerName gameId _ ->
            div []
                [ h1 [] [ text "Outsixer" ]
                , nameInput playerName
                , joinGame gameId
                , createNewGame
                ]

        Lobby { player, players, gameId } _ ->
            div []
                [ text <| "Lobby : " ++ String.fromInt gameId
                , h1 [] [ text "players" ]
                , listPlayers players
                , startGame
                ]

        InGame { player, players, word } _ ->
            div []
                [ text "Playing Game..."
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
        , input [ onInput UpdateGameId ] [ text gameName ]
        , button [ onClick JoinLobby ] [ text "Join Game" ]
        ]


createNewGame : Html Msg
createNewGame =
    div []
        [ button [ onClick CreateGame ] [ text "Create Game" ]
        ]


startGame : Html Msg
startGame =
    div []
        [ button [ onClick StartGame ] [ text "Start Game" ]
        ]


listPlayers : List Player -> Html msg
listPlayers players =
    div [] (List.map playerListItem players)


playerListItem : Player -> Html msg
playerListItem player =
    div [] [ text player.name ]


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
        , subscriptions = subscriptions
        }
