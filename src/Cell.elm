module Cell exposing (Cell, CellState(..), Msg, renderCell, updateCell)

import Html exposing (Html, td, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)


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


updateCell : Msg -> Cell -> Cell
updateCell msg cell =
    case msg of
        CellStartHover ->
            { cell | isHovering = True }

        CellStopHover ->
            { cell | isHovering = False }



-- VIEW


renderCell : CellComponent -> Html Msg
renderCell { cell } =
    td
        [ style "width" "30px"
        , style "height" "30px"
        , style "border-radius" "3px"
        , style "background-color"
            (if cell.isHovering then
                "#d3d7cf"

             else
                "#babdb6"
            )
        , onMouseEnter CellStartHover
        , onMouseLeave CellStopHover
        ]
        []
