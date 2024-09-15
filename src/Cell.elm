module Cell exposing (Msg, renderCellComponent, updateCellComponent)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave, preventDefaultOn)
import Json.Decode as Decode
import Types exposing (CellComponent, CellState(..), Minefield)
import Utils exposing (revealCell, updateCell)



-- UPDATE


type Msg
    = CellStartHover
    | CellStopHover
    | Flag
    | Reveal


updateCellComponent : Msg -> Int -> Int -> Minefield -> Minefield
updateCellComponent msg row col minefield =
    case msg of
        CellStartHover ->
            updateCell row col (\cell -> { cell | isHovering = True }) minefield

        CellStopHover ->
            updateCell row col (\cell -> { cell | isHovering = False }) minefield

        Flag ->
            updateCell row col (\cell -> { cell | flagged = not cell.flagged }) minefield

        Reveal ->
            Debug.log "revealed" (revealCell col row minefield)



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
            , style "font-size" "24px"
            , style "font-weight" "bold"
            , style "cursor" "default"
            , style "color"
                (case cell.surroundingMines of
                    1 ->
                        "blue"

                    2 ->
                        "green"

                    3 ->
                        "red"

                    4 ->
                        "purple"

                    5 ->
                        "maroon"

                    _ ->
                        "black"
                )
            , style "background-color"
                (if cell.isHovering then
                    "#d3d7cf"

                 else if cell.state == Unopened || cell.state == Flagged then
                    "#babdb6"

                 else
                    -- Opened
                    "#d8dad6"
                )
            , onMouseEnter CellStartHover
            , onMouseLeave CellStopHover
            , onClick Reveal
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
