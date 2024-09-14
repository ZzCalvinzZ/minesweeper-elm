module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Cell exposing (Cell)
import Dict exposing (diff)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import Url
import Utils exposing (GameConfig, Minefield, generateMinefield)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type Difficulty
    = Beginner
    | Intermediate
    | Expert


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , difficulty : Maybe Difficulty
    , gameConfig : Maybe GameConfig
    , minefield : Maybe Minefield
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url Nothing Nothing Nothing, Cmd.none )



-- UPDATE


type Msg
    = Start Difficulty
    | CellMsgReceived Int Int Cell.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | MinefieldGenerated Minefield


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start difficulty ->
            let
                config =
                    getConfigForDifficulty difficulty
            in
            ( { model | gameConfig = Just config, difficulty = Just difficulty }, Random.generate MinefieldGenerated (generateMinefield config) )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        CellMsgReceived row col cell ->
            ( model, Cmd.none )

        MinefieldGenerated minefield ->
            ( { model | minefield = Just minefield }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Minesweeper"
    , body =
        [ div [ style "textAlign" "center" ]
            [ h1 [] [ text "Minesweeper" ]
            , h2 [] [ text (subHeaderText model.difficulty) ]
            , div []
                (case model.minefield of
                    Just minefield ->
                        gameView minefield

                    Nothing ->
                        initialView model
                )
            ]
        ]
    }


initialView : Model -> List (Html Msg)
initialView model =
    [ button [ onClick (Start Beginner) ] [ text "Beginner" ]
    , button [ onClick (Start Intermediate) ] [ text "Intermediate" ]
    , button [ onClick (Start Expert) ] [ text "Expert" ]
    ]


gameView : Minefield -> List (Html Msg)
gameView minefield =
    [ div
        [ style "display" "flex", style "justify-content" "center" ]
        [ table []
            [ tbody []
                (List.indexedMap renderRow minefield)
            ]
        ]
    ]



-- Render a row (tr) by mapping over the cells in that row


renderRow : Int -> List Cell -> Html Msg
renderRow rowIndex row =
    tr []
        (List.indexedMap (renderCell rowIndex) row)



-- Render a cell (td) by passing the row and column indexes


renderCell : Int -> Int -> Cell -> Html Msg
renderCell rowIndex colIndex cell =
    Html.map (CellMsgReceived rowIndex colIndex) (Cell.renderCell { rowIndex = rowIndex, colIndex = colIndex, cell = cell })



-- subheader text


subHeaderText : Maybe Difficulty -> String
subHeaderText difficulty =
    case difficulty of
        Just Beginner ->
            "Beginner"

        Just Intermediate ->
            "Intermediate"

        Just Expert ->
            "Expert"

        Nothing ->
            "Pick a level"



-- get the appropriate config for the difficulty level


getConfigForDifficulty : Difficulty -> GameConfig
getConfigForDifficulty difficulty =
    case difficulty of
        Beginner ->
            { rows = 8, columns = 8, mines = 10 }

        Intermediate ->
            { rows = 16, columns = 16, mines = 40 }

        Expert ->
            { rows = 16, columns = 30, mines = 99 }
