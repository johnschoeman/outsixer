module Main exposing (..)

import API exposing (GraphQLResponse)
import Browser
import Env exposing (Env)
import Game exposing (Game)
import Graphql.Http
import Html exposing (Html, button, div, h1, img, input, label, text)
import Html.Attributes exposing (disabled, src)
import Html.Events exposing (onClick, onInput)
import Player exposing (Player, Role)
import RemoteData exposing (RemoteData)
import Time



---- MODEL ----


type Model
    = PreGame PlayerName GameId Loading Env
    | Lobby LobbyState Env
    | InGame GameState Env


type alias Loading =
    Bool


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
    , game : Game
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
    ( PreGame "" "" False env
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
    | ExitLobby
    | ReceivedGameResponse API.GameResponse
    | StartGame
    | ReceivedStartGameResponse API.StartGameResponse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( PreGame _ gameId loading env, UpdatePlayerName playerName ) ->
            ( PreGame playerName gameId loading env, Cmd.none )

        ( PreGame playerName _ loading env, UpdateGameId gameId ) ->
            ( PreGame playerName gameId loading env, Cmd.none )

        ( PreGame playerName gameId loading env, CreateGame ) ->
            let
                gameName =
                    "GAME"
            in
            if String.length playerName > 0 then
                ( PreGame playerName gameId True env, API.createGame env gameName ReceivedCreateGameResponse )

            else
                ( model, Cmd.none )

        ( PreGame playerName _ loading env, ReceivedCreateGameResponse response ) ->
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

        ( PreGame playerName gameId loading env, JoinLobby ) ->
            case String.toInt gameId of
                Just id ->
                    if String.length playerName > 0 then
                        ( PreGame playerName gameId True env, API.joinLobby env playerName id ReceivedJoinLobbyResponse )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ( PreGame playerName gameId loading env, ReceivedJoinLobbyResponse response ) ->
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
                                    ( PreGame playerName gameId False env, Cmd.none )

                        Nothing ->
                            ( PreGame playerName gameId False env, Cmd.none )

                _ ->
                    ( PreGame playerName gameId False env, Cmd.none )

        ( PreGame _ _ _ _, _ ) ->
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
                                        , game =
                                            { id = game.id
                                            , players = game.players
                                            , word = game.word
                                            , name = game.name
                                            , active = game.active
                                            }
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

        ( Lobby { player, gameId } env, ExitLobby ) ->
            ( PreGame player.name (String.fromInt gameId) False env, Cmd.none )

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
        PreGame playerName gameId loading _ ->
            preGameScreen playerName gameId loading

        Lobby { player, players, gameId } _ ->
            lobbyScreen player players gameId

        InGame { player, game } _ ->
            div []
                [ text "Playing Game..."
                ]



---- PREGAME SCREEN ----


preGameScreen : String -> String -> Bool -> Html Msg
preGameScreen playerName gameId loading =
    div []
        [ h1 [] [ text "Outsixer" ]
        , nameInput playerName loading
        , joinGame gameId loading
        , createNewGame loading
        , loadingIndicator loading
        ]


nameInput : String -> Bool -> Html Msg
nameInput name loading =
    div []
        [ label [] [ text "Player Name: " ]
        , input [ onInput UpdatePlayerName, disabled loading ] [ text name ]
        ]


joinGame : String -> Bool -> Html Msg
joinGame gameName loading =
    div []
        [ label [] [ text "Game Name: " ]
        , input [ onInput UpdateGameId, disabled loading ] [ text gameName ]
        , button [ onClick JoinLobby, disabled loading ] [ text "Join Game" ]
        ]


createNewGame : Bool -> Html Msg
createNewGame loading =
    div []
        [ button [ onClick CreateGame, disabled loading ] [ text "Create Game" ]
        ]


loadingIndicator : Bool -> Html Msg
loadingIndicator loading =
    if loading then
        div [] [ text "loading..." ]

    else
        text ""



---- LOBBY SCREEN ----


lobbyScreen : Player -> List Player -> Int -> Html Msg
lobbyScreen player players gameId =
    div []
        [ text <| "Lobby : " ++ String.fromInt gameId
        , h1 [] [ text "players" ]
        , listPlayers players
        , startGame
        , exitLobby
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


exitLobby : Html Msg
exitLobby =
    button [ onClick ExitLobby ] [ text "Exit Lobby" ]


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
