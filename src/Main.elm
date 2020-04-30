module Main exposing (..)

import API
import Browser
import Env exposing (Env)
import Game exposing (Game)
import Graphql.Http
import Html exposing (Html, button, div, h1, img, input, label, p, text)
import Html.Attributes exposing (disabled, src, value)
import Html.Events exposing (onClick, onInput)
import Player exposing (Player)
import Random
import RemoteData
import Time
import Word



---- MODEL ----


type Model
    = PreGame PlayerName GameId Loading Env
    | Lobby LobbyState Env
    | InGame GameState ClockTime Env


type alias Loading =
    Bool


type alias PlayerName =
    String


type alias GameId =
    String


type alias Word =
    String


type alias ClockTime =
    Int


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
    | ReceivedExitLobbyResponse API.ExitLobbyResponse
    | ReceivedGameResponse API.GameResponse
    | StartGame
    | GenerateGameData ( Int, String )
    | ReceivedStartGameResponse API.StartGameResponse
    | ReceivedAssignRoleResponse API.AssignRoleResponse
    | EndGame
    | ReceivedEndGameResponse API.EndGameResponse


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
                                    player =
                                        case List.head <| List.filter (\p -> p.id == lobbyData.player.id) game.players of
                                            Just p ->
                                                p

                                            Nothing ->
                                                lobbyData.player

                                    gameData =
                                        { player = player
                                        , game =
                                            { id = game.id
                                            , players = game.players
                                            , word = game.word
                                            , name = game.name
                                            , active = game.active
                                            }
                                        }
                                in
                                ( InGame gameData 300 env, Cmd.none )

                            else
                                ( Lobby { lobbyData | players = game.players } env, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( Lobby lobbyData env, StartGame ) ->
            let
                randomData =
                    Random.pair (Random.int Random.minInt Random.maxInt) Word.randomWord
            in
            ( model
            , Random.generate GenerateGameData randomData
            )

        ( Lobby lobbyData env, GenerateGameData ( seedInt, word ) ) ->
            let
                playersWithRoles =
                    Player.assignRoles seedInt lobbyData.players
            in
            ( model, startGame env lobbyData.gameId word playersWithRoles )

        ( Lobby { player } env, ExitLobby ) ->
            ( model
            , API.exitLobby env player.id ReceivedExitLobbyResponse
            )

        ( Lobby { player, gameId } env, ReceivedExitLobbyResponse resposne ) ->
            case resposne of
                RemoteData.Success _ ->
                    ( PreGame player.name (String.fromInt gameId) False env, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( Lobby _ _, _ ) ->
            ( model, Cmd.none )

        ( InGame gameState clockTime env, Tick _ ) ->
            let
                game =
                    gameState.game

                nextTime =
                    clockTime - 1
            in
            ( InGame gameState nextTime env, API.fetchGame env game.id ReceivedGameResponse )

        ( InGame { player } _ env, ReceivedGameResponse response ) ->
            case response of
                RemoteData.Success responseData ->
                    case List.head responseData of
                        Just game ->
                            if not game.active then
                                ( Lobby { player = player, players = game.players, gameId = game.id } env, Cmd.none )

                            else
                                ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( InGame { game } _ env, EndGame ) ->
            ( model, endGame env game )

        ( InGame { player, game } _ env, ReceivedEndGameResponse response ) ->
            case response of
                RemoteData.Success data ->
                    ( Lobby { player = player, players = game.players, gameId = game.id } env, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( InGame _ _ _, _ ) ->
            ( model, Cmd.none )


startGame : Env -> Int -> String -> List Player -> Cmd Msg
startGame env gameId word players =
    Cmd.batch <|
        API.startGame env gameId word ReceivedStartGameResponse
            :: List.map (assignRoleMsg env) players


assignRoleMsg : Env -> Player -> Cmd Msg
assignRoleMsg env player =
    API.assignPlayerRole env player.id player.role ReceivedAssignRoleResponse


endGame : Env -> Game -> Cmd Msg
endGame env game =
    API.endGame env game.id ReceivedEndGameResponse



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

        InGame { player, game } clockTime _ ->
            gameScreen player game clockTime



---- PREGAME SCREEN ----


preGameScreen : String -> String -> Bool -> Html Msg
preGameScreen playerName gameId loading =
    div []
        [ h1 [] [ text "Outsixer" ]
        , nameInput playerName loading
        , joinGame gameId playerName loading
        , createNewGame playerName loading
        , loadingIndicator loading
        ]


nameInput : String -> Bool -> Html Msg
nameInput name loading =
    div []
        [ label [] [ text "Player Name: " ]
        , input [ onInput UpdatePlayerName, disabled loading, value name ] [ text name ]
        ]


joinGame : String -> String -> Bool -> Html Msg
joinGame gameName playerName loading =
    let
        d =
            loading || (String.length gameName == 0) || (String.length playerName == 0)
    in
    div []
        [ label [] [ text "Game Name: " ]
        , input [ onInput UpdateGameId ] [ text gameName ]
        , button [ onClick JoinLobby, disabled d ] [ text "Join Game" ]
        ]


createNewGame : String -> Bool -> Html Msg
createNewGame playerName loading =
    let
        d =
            loading || (String.length playerName == 0)
    in
    div []
        [ button [ onClick CreateGame, disabled d ] [ text "Create Game" ]
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
        , startGameButton
        , exitLobby
        ]


startGameButton : Html Msg
startGameButton =
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



---- GAME SCREEN ----


gameScreen : Player -> Game -> ClockTime -> Html Msg
gameScreen player game clockTime =
    div []
        [ h1 [] [ text "Outsixer" ]
        , playerInfo player game.word
        , endGameButton
        , timeRemaining clockTime
        ]


playerInfo : Player -> String -> Html msg
playerInfo player word =
    case player.role of
        Just "Master" ->
            div []
                [ text "you are the Master"
                , p [] [ text <| "the word is " ++ word ]
                ]

        Just "Insider" ->
            div []
                [ text "you are the Insider"
                , p [] [ text <| "the word is " ++ word ]
                ]

        Just "Commoner" ->
            div [] [ text "you are a Commoner" ]

        _ ->
            div [] [ text "This game has already started" ]


endGameButton : Html Msg
endGameButton =
    button [ onClick EndGame ] [ text "End Game" ]


timeRemaining : ClockTime -> Html msg
timeRemaining clockTime =
    let
        minutes =
            String.fromInt <| clockTime // 60

        seconds =
            String.padLeft 2 '0' (String.fromInt <| modBy 60 clockTime)
    in
    if clockTime > 0 then
        div [] [ text <| minutes ++ " : " ++ seconds ]

    else
        div [] [ text "Game over! Can you guess the insider?" ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.document
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
