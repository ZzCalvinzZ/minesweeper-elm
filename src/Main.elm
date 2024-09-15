module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Cell
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import Types exposing (CellState(..), CellType, Difficulty(..), GameConfig, Minefield)
import Url
import Utils exposing (generateMinefield)



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


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , difficulty : Maybe Difficulty
    , gameConfig : Maybe GameConfig
    , minefield : Maybe Minefield
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
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

        CellMsgReceived row col cellMsg ->
            let
                newMinefield =
                    case model.minefield of
                        Just minefield ->
                            Just (Cell.updateCellComponent cellMsg row col minefield)

                        -- Just (updateCell row col (Cell.updateCellComponent cellMsg) minefield)
                        Nothing ->
                            Nothing
            in
            ( { model | minefield = newMinefield }, Cmd.none )

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
initialView _ =
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


renderRow : Int -> List CellType -> Html Msg
renderRow rowIndex row =
    tr []
        (List.indexedMap (renderCell rowIndex) row)



-- Render a cell (td) by passing the row and column indexes


renderCell : Int -> Int -> CellType -> Html Msg
renderCell rowIndex colIndex cell =
    Html.map (CellMsgReceived rowIndex colIndex) (Cell.renderCellComponent { rowIndex = rowIndex, colIndex = colIndex, cell = cell })



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
