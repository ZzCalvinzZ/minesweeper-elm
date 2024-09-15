module Cell exposing (Cell, CellState(..), Msg, renderCellComponent, updateCellComponent)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave, preventDefaultOn)
import Json.Decode as Decode


type CellState
    = Unopened
    | Opened
    | Flagged


type alias Cell =
    { state : CellState
    , hasMine : Bool
    , flagged : Bool
    , surroundingMines : Int
    , isHovering : Bool
    }


type alias CellComponent =
    { rowIndex : Int
    , colIndex : Int
    , cell : Cell
    }



-- UPDATE


type Msg
    = CellStartHover
    | CellStopHover
    | Flag


updateCellComponent : Msg -> Cell -> Cell
updateCellComponent msg cell =
    case msg of
        CellStartHover ->
            { cell | isHovering = True }

        CellStopHover ->
            { cell | isHovering = False }

        Flag ->
            { cell | flagged = not cell.flagged }



-- VIEW


renderCellComponent : CellComponent -> Html Msg
renderCellComponent { cell } =
    td
        []
        [ div
            [ style "width" "30px"
            , style "height" "30px"
            , style "border-radius" "3px"
            , style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            , style "background-color"
                (if cell.isHovering then
                    "#d3d7cf"

                 else
                    "#babdb6"
                )
            , onMouseEnter CellStartHover
            , onMouseLeave CellStopHover
            , preventDefaultOn "contextmenu" (Decode.succeed ( Flag, True ))
            ]
            (if cell.flagged then
                [ img
                    [ style "width" "70%"
                    , style "height" "70%"
                    , style "margin-left" "6px"
                    , alt "flag"
                    , src "./flag.png"
                    ]
                    []
                ]

             else
                []
            )
        ]
