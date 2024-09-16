module Cell exposing (Msg, renderCellComponent, updateCellComponent)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave, preventDefaultOn)
import Json.Decode as Decode
import Types exposing (CellComponent, CellState(..), CellType, GameStatus(..), Minefield, Model)
import Utils exposing (checkGameStatus, getCell, revealCell, revealSurrounding, updateCell)



-- UPDATE


type Msg
    = CellStartHover
    | CellStopHover
    | FlagOrRevealAllUnflagged
    | Reveal


updateCellComponent : Msg -> Int -> Int -> Model -> Model
updateCellComponent msg row col model =
    case model.minefield of
        -- if no minefield exists, don't handle any messages
        Nothing ->
            model

        Just minefield ->
            case msg of
                CellStartHover ->
                    { model | minefield = Just (updateCell row col (\c -> { c | isHovering = True }) minefield) }

                CellStopHover ->
                    { model | minefield = Just (updateCell row col (\c -> { c | isHovering = False }) minefield) }

                FlagOrRevealAllUnflagged ->
                    let
                        maybeCell =
                            getCell col row minefield
                    in
                    case maybeCell of
                        Nothing ->
                            { model | minefield = Just minefield }

                        Just cell ->
                            case cell.state of
                                -- if opened, we reveal all unflagged cells around the current one
                                Opened ->
                                    let
                                        newMinefield =
                                            revealSurrounding col row minefield
                                    in
                                    { model
                                        | minefield = Just newMinefield
                                        , gameStatus = checkGameStatus newMinefield
                                    }

                                -- if unopened or flagged just add flag or remove flag
                                Unopened ->
                                    { model | minefield = Just (updateCell row col (\c -> { c | state = Flagged }) minefield) }

                                Flagged ->
                                    { model | minefield = Just (updateCell row col (\c -> { c | state = Unopened }) minefield) }

                -- reveal a cell that was clicked
                Reveal ->
                    case getCell col row minefield of
                        -- only reveal if a real cell was clicked
                        Nothing ->
                            model

                        Just cell ->
                            case cell.state of
                                -- only reveal if the cell is currently unopened
                                Unopened ->
                                    let
                                        newMinefield =
                                            revealCell col row minefield
                                    in
                                    { model
                                        | minefield = Just newMinefield
                                        , gameStatus = checkGameStatus newMinefield
                                    }

                                _ ->
                                    model



-- VIEW


renderCellComponent : CellComponent -> Html Msg
renderCellComponent { model, cell } =
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
                (if model.gameStatus == Won && cell.hasMine == True then
                    "lime"

                 else if model.gameStatus == Lost && cell.hasMine == True then
                    "red"

                 else if cell.isHovering then
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
            , preventDefaultOn "contextmenu" (Decode.succeed ( FlagOrRevealAllUnflagged, True ))
            ]
            (rendercellContent model cell)
        ]


rendercellContent : Model -> CellType -> List (Html msg)
rendercellContent model cell =
    case cell.state of
        Flagged ->
            if gameIsWonOrLost model && cell.hasMine == True then
                renderMine

            else
                renderFlag

        Unopened ->
            if gameIsWonOrLost model && cell.hasMine == True then
                renderMine

            else
                []

        Opened ->
            if model.gameStatus == Lost && cell.hasMine == True then
                renderMine

            else
                [ text
                    (if cell.surroundingMines > 0 then
                        String.fromInt cell.surroundingMines

                     else
                        ""
                    )
                ]


renderFlag : List (Html msg)
renderFlag =
    [ img
        [ style "width" "70%"
        , style "height" "70%"
        , style "margin-left" "6px"
        , alt "🚩"
        , src "./flag.png"
        ]
        []
    ]


renderMine : List (Html msg)
renderMine =
    [ img
        [ style "width" "70%"
        , style "height" "70%"
        , alt "Mine"
        , src "./mine.png"
        ]
        []
    ]


gameIsWonOrLost : Model -> Bool
gameIsWonOrLost model =
    model.gameStatus == Lost || model.gameStatus == Won
