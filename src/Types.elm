module Types exposing (..)

import Browser.Navigation as Nav
import Url


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
    , surroundingMines : Int
    , isHovering : Bool
    }


type alias CellComponent =
    { model : Model
    , rowIndex : Int
    , colIndex : Int
    , cell : CellType
    }


type Difficulty
    = Beginner
    | Intermediate
    | Expert


type GameStatus
    = Select
    | Started
    | Won
    | Lost


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , difficulty : Maybe Difficulty
    , gameConfig : Maybe GameConfig
    , minefield : Maybe Minefield
    , gameStatus : GameStatus
    }
