module Types exposing (..)


type alias Minefield =
    List (List CellType)


type alias GameConfig =
    { columns : Int
    , rows : Int
    , mines : Int
    }


type CellState
    = Unopened
    | Opened
    | Flagged


type alias CellType =
    { state : CellState
    , hasMine : Bool
    , flagged : Bool
    , surroundingMines : Int
    , isHovering : Bool
    }


type alias CellComponent =
    { rowIndex : Int
    , colIndex : Int
    , cell : CellType
    }


type Difficulty
    = Beginner
    | Intermediate
    | Expert
