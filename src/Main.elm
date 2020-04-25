module Main exposing (..)

import API
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
    = Loading Env
    | Error (Graphql.Http.Error (List Game)) Env
    | Game (List Game) Env


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
    ( Loading env
    , API.fetchGames env ReceivedGames
    )



---- UPDATE ----


type Msg
    = CreateGame
    | ReceivedGames (RemoteData (Graphql.Http.Error (List Game)) (List Game))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Loading env, ReceivedGames response ) ->
            case response of
                RemoteData.Success games ->
                    ( Game games env, Cmd.none )

                RemoteData.Failure error ->
                    ( Error error env, Cmd.none )

                RemoteData.Loading ->
                    ( Loading env, Cmd.none )

                RemoteData.NotAsked ->
                    ( model, Cmd.none )

        ( Game games env, CreateGame ) ->
            ( model, Cmd.none )

        ( Error _ _, _ ) ->
            ( model, Cmd.none )

        ( Loading _, _ ) ->
            ( model, Cmd.none )

        ( Game _ _, _ ) ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Outsixer", body = [ page model ] }


page : Model -> Html Msg
page model =
    case model of
        Loading _ ->
            div [] [ text "loading" ]

        Error error _ ->
            errorMessage error

        Game games _ ->
            div []
                [ h1 [] [ text "Outsixer" ]
                , button [ onClick CreateGame ] [ text "CreateGame" ]
                , listGames games
                ]


errorMessage : Graphql.Http.Error d -> Html Msg
errorMessage error =
    case error of
        Graphql.Http.GraphqlError p graphqlErrors ->
            div [] [ text "graphqlError" ]

        Graphql.Http.HttpError httpError ->
            div [] [ text <| API.showHttpError httpError ]


listGames : List Game -> Html msg
listGames games =
    div [] (List.map gameListItem games)


gameListItem : Game -> Html msg
gameListItem game =
    div []
        [ text game.name
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.document
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
