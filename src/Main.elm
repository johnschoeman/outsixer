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
    { player : GraphQLResponse Player
    , players : GraphQLResponse (List Player)
    , gameId : Int
    }


type alias GameState =
    { player : Player
    , players : List Player
    , word : String
    }


type alias Player =
    { id : Int
    , name : String
    , role : Maybe Role
    }


type Role
    = Master
    | Insider
    | Commoner


type alias GraphQLResponse data =
    RemoteData (Graphql.Http.Error data) data


type alias GameData =
    GraphQLResponse (List Game)


type alias JoinGameResponseData =
    { player : Player
    , players : List Player
    }


type alias JoinGameResponse =
    GraphQLResponse JoinGameResponseData


type alias StartGameResponseData =
    { player : Player
    , players : List Player
    }


type alias StartGameResponse =
    GraphQLResponse StartGameResponseData


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
    = UpdatePlayerName String
    | UpdateGameId String
    | CreateGame
    | ReceivedCreateGameResponse API.CreateGameResponse
    | JoinGame
    | ReceivedJoinGameResponse JoinGameResponse
    | StartGame
    | ReceivedStartGameResponse StartGameResponse


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

        ( PreGame playerName gameId env, ReceivedCreateGameResponse response ) ->
            case response of
                RemoteData.Success createGameData ->
                    let
                        lobbyData =
                            { player = RemoteData.NotAsked
                            , players = RemoteData.NotAsked
                            , gameId = createGameData.id
                            }
                    in
                    -- ( Lobby lobbyData env, API.joinGame env gameId playerName )
                    ( Lobby lobbyData env, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( PreGame playerName gameId env, JoinGame ) ->
            case String.toInt gameId of
                Just id ->
                    -- ( model, API.joinGame env playerName id ReceivedJoinGameResponse )
                    ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ( PreGame _ _ env, ReceivedJoinGameResponse response ) ->
            case response of
                RemoteData.Success joinGameData ->
                    let
                        player =
                            joinGameData.player

                        players =
                            joinGameData.players

                        lobbyData =
                            -- { player = player, players = players, gameId = 1 }
                            { player = RemoteData.NotAsked, players = RemoteData.NotAsked, gameId = 1 }
                    in
                    ( Lobby lobbyData env, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( PreGame _ _ _, _ ) ->
            ( model, Cmd.none )

        ( Lobby lobbyData env, StartGame ) ->
            -- ( model, API.startGame env lobbyData.gameId ReceivedStartGameResponse )
            ( model, Cmd.none )

        ( Lobby _ env, ReceivedStartGameResponse response ) ->
            case response of
                RemoteData.Success startGameData ->
                    let
                        player =
                            { id = 1, name = "name", role = Just Master }

                        players =
                            [ player ]

                        gameData =
                            { player = player, players = players, word = "Bunny" }
                    in
                    ( InGame gameData env, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( Lobby _ _, _ ) ->
            ( model, Cmd.none )

        ( InGame _ _, _ ) ->
            ( model, Cmd.none )



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

        Lobby { player, players } _ ->
            div []
                [ text "Lobby :"
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
        , button [ onClick JoinGame ] [ text "Join Game" ]
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


listPlayers : GraphQLResponse (List Player) -> Html msg
listPlayers players =
    div [] [ text "list Players " ]


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
