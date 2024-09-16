module Utils exposing (anyCellOpenedWithMine, generateMinefield, getCell, revealCell, revealSurrounding, updateCell)

import List exposing (indexedMap, repeat)
import List.Extra
import Random exposing (Generator, int)
import Types exposing (CellState(..), CellType, GameConfig, Minefield)



-- Default cell (no mine, not revealed, no surrounding mines)


defaultCell : CellType
defaultCell =
    { state = Unopened
    , hasMine = False
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


updateCell : Int -> Int -> (CellType -> CellType) -> Minefield -> Minefield
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



-- Reveal a cell by also revealing neighbors if they can safely be revealed


revealCell : Int -> Int -> Minefield -> Minefield
revealCell col row minefield =
    let
        -- Get the main cell at the target row/col
        mainCell =
            getCell col row minefield

        -- Check the surrounding cells
        columns =
            [ col - 1, col, col + 1 ]

        rows =
            [ row - 1, row, row + 1 ]
    in
    case mainCell of
        Nothing ->
            minefield

        Just cell ->
            if cell.state == Opened || cell.state == Flagged then
                minefield

            else
                let
                    -- Count the surrounding mines
                    surroundingMines =
                        countSurroundingMines columns rows minefield

                    -- Update the minefield with the revealed cell
                    newMinefield =
                        updateCell row col (\_ -> updateMainCell { cell | surroundingMines = surroundingMines }) minefield
                in
                if surroundingMines == 0 then
                    -- If no surrounding mines, reveal surrounding cells
                    revealSurroundingCells columns rows newMinefield

                else
                    -- Otherwise, just return the updated minefield
                    newMinefield



-- Function to count surrounding mines


countSurroundingMines : List Int -> List Int -> Minefield -> Int
countSurroundingMines cols rows mf =
    List.foldl
        (\c acc ->
            List.foldl
                (\r innerAcc ->
                    case getCell c r mf of
                        Just cell ->
                            if cell.hasMine then
                                innerAcc + 1

                            else
                                innerAcc

                        Nothing ->
                            innerAcc
                )
                acc
                rows
        )
        0
        cols



-- Update cell in the minefield


updateMainCell : CellType -> CellType
updateMainCell cell =
    { cell | state = Opened }



-- Reveal the surrounding cells if no surrounding mines


revealSurroundingCells : List Int -> List Int -> Minefield -> Minefield
revealSurroundingCells cols rows mf =
    List.foldl
        (\c accMinefield ->
            List.foldl
                (\r innerMinefield ->
                    revealCell c r innerMinefield
                )
                accMinefield
                rows
        )
        mf
        cols



-- Helper to get a cell safely, returning Maybe Cell


getCell : Int -> Int -> Minefield -> Maybe CellType
getCell c r mf =
    case List.Extra.getAt r mf of
        Just rowList ->
            List.Extra.getAt c rowList

        Nothing ->
            Nothing


revealSurrounding : Int -> Int -> Minefield -> Minefield
revealSurrounding column row minefield =
    let
        columns =
            [ column - 1, column, column + 1 ]

        rows =
            [ row - 1, row, row + 1 ]
    in
    List.foldl
        (\c accMinefield ->
            List.foldl
                (\r innerMinefield ->
                    revealCell c r innerMinefield
                )
                accMinefield
                rows
        )
        minefield
        columns


anyCellOpenedWithMine : Minefield -> Bool
anyCellOpenedWithMine minefield =
    List.any
        (\row ->
            List.any
                (\cell ->
                    cell.state == Opened && cell.hasMine
                )
                row
        )
        minefield
