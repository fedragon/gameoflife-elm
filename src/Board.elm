module Board exposing (..)

import Dict exposing (Dict)
import Cell

type alias Model = Dict (Int, Int) Cell.Model

empty : Model
empty = Dict.empty

init : Int -> Int -> Model
init boardSide cellSize =
  Dict.fromList(
    List.concatMap
      (\x ->
        List.map
          (\y -> ((x, y), (Cell.init x y cellSize)))
          (List.range 0 boardSide))
      (List.range 0 boardSide))

makeAlive : Dict (Int, Int) Int -> Model -> Model
makeAlive these board =
  Dict.map
    (\coords cell ->
      Dict.get coords these
      |> Maybe.map (\color -> Cell.update (Cell.Resurrect color) cell)
      |> Maybe.withDefault cell)
    board

update : Model -> Model
update board =
  Dict.map
    (\_ cell -> (updateCell cell board))
    board

updateCell : Cell.Model -> Model -> Cell.Model
updateCell cell cells =
  let
    neighbours = aliveNeighboursOf cell cells
  in
    (Cell.update (Cell.Evolve neighbours) cell)

aliveNeighboursOf : Cell.Model -> Model -> List Cell.Model
aliveNeighboursOf cell cells =
  let
    adjacent = [
      (cell.x - 1, cell.y - 1),
      (cell.x    , cell.y - 1),
      (cell.x + 1, cell.y - 1),
      (cell.x - 1, cell.y),
      (cell.x + 1, cell.y),
      (cell.x - 1, cell.y + 1),
      (cell.x    , cell.y + 1),
      (cell.x + 1, cell.y + 1)
    ]
  in
    List.filter
      (.alive)
      (getAll adjacent cells)

getAll : List (Int, Int) -> Model -> List Cell.Model
getAll these from =
  List.filterMap
    (\t -> Dict.get t from)
    these