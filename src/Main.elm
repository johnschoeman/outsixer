module Main exposing (..)

import API exposing (GraphQLResponse)
import Browser
import Env exposing (Env)
import Game exposing (Game)
import Graphql.Http
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import RemoteData exposing (RemoteData)



---- MODEL ----


type Model
    = MainApp AppData Env


type alias AppData =
    { games : GameData
    , gameName : CreateGameData
    }


type alias GameData =
    RemoteData (Graphql.Http.Error (List Game)) (List Game)


type alias MutationResponse =
    { affected_rows : Int }


type alias CreateGameData =
    RemoteData (Graphql.Http.Error (Maybe MutationResponse)) (Maybe MutationResponse)


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
        , gameName = RemoteData.NotAsked
        }
        env
    , API.fetchGames env ReceivedGames
    )



---- UPDATE ----


type Msg
    = CreateGame
    | ReceivedGames GameData
    | ReceivedCreateGameResponse CreateGameData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( MainApp appData env, ReceivedGames gameData ) ->
            ( MainApp { appData | games = gameData } env, Cmd.none )

        ( MainApp _ env, CreateGame ) ->
            ( model, API.createGame env ReceivedCreateGameResponse )

        ( MainApp appData env, ReceivedCreateGameResponse createGameData ) ->
            ( MainApp { appData | gameName = createGameData } env, Cmd.none )



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
                , button [ onClick CreateGame ] [ text "CreateGame" ]
                , listGames appData.games
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
