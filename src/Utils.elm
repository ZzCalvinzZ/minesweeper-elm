module Utils exposing (GameConfig, Minefield, generateMinefield, updateCell)

import Cell exposing (Cell, CellState(..))
import List exposing (indexedMap, repeat)
import Random exposing (Generator, int)


type alias Minefield =
    List (List Cell)


type alias GameConfig =
    { columns : Int
    , rows : Int
    , mines : Int
    }



-- Default cell (no mine, not revealed, no surrounding mines)


defaultCell : Cell
defaultCell =
    { state = Unopened
    , hasMine = False
    , flagged = False
    , surroundingMines = 0
    , isHovering = False
    }



-- Create an empty minefield based on the configuration


createEmptyMinefield : Int -> Int -> Minefield
createEmptyMinefield rows columns =
    repeat rows (repeat columns defaultCell)



-- Place a single mine at a random location in the minefield


placeMine : Int -> Int -> Minefield -> Generator Minefield
placeMine rows columns minefield =
    let
        rowGen =
            int 0 (rows - 1)

        colGen =
            int 0 (columns - 1)
    in
    Random.map2
        (\row col ->
            updateCell row col (\cell -> { cell | hasMine = True }) minefield
        )
        rowGen
        colGen



-- Place multiple mines in the minefield using a list of generators


placeMines : Int -> Int -> Int -> Minefield -> Generator Minefield
placeMines rows columns remainingMines minefield =
    if remainingMines <= 0 then
        Random.constant minefield

    else
        Random.andThen
            (\newMinefield ->
                placeMines rows columns (remainingMines - 1) newMinefield
            )
            (placeMine rows columns minefield)



-- Update a specific cell in the minefield


updateCell : Int -> Int -> (Cell -> Cell) -> Minefield -> Minefield
updateCell row col updateFunc minefield =
    indexedMap
        (\rIndex rowList ->
            if rIndex == row then
                indexedMap
                    (\cIndex cell ->
                        if cIndex == col then
                            updateFunc cell

                        else
                            cell
                    )
                    rowList

            else
                rowList
        )
        minefield



-- Generate a complete minefield with mines placed


generateMinefield : GameConfig -> Generator Minefield
generateMinefield config =
    let
        emptyMinefield =
            createEmptyMinefield config.rows config.columns
    in
    placeMines config.rows config.columns config.mines emptyMinefield
