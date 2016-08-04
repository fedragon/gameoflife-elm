import Cell
import Html exposing (Html, button, div, span, text)
import Html.App as App
import Html.Events exposing (onClick)
import Random
import Svg exposing (svg)
import Svg.Attributes exposing (height, viewBox, width)
import Time exposing (Time, every, millisecond)

type alias Model =
  { generation : Int
  , cells : List Cell.Model
  , paused : Bool
  }

type Msg =
  NextGen Time
  | Restart
  | Pause
  | RaiseFromTheDead (List (Int, Int))

main =
  App.program {
    init = init,
    update = update,
    subscriptions = subscriptions,
    view = view
  }

raiseFromTheDead : Int -> Int -> Cmd Msg
raiseFromTheDead total boardSide =
  Random.generate
    RaiseFromTheDead
    (Random.list
      total
      (Random.pair
        (Random.int 1 boardSide)
        (Random.int 1 boardSide)))

init : (Model, Cmd Msg)
init =
  let
    boardSide = 30
    cellSize = 15
    initiallyAlive = boardSide * 3
    board =
      List.concatMap
        (\x ->
          List.map
            (\y -> (Cell.init x y cellSize False))
            [0..boardSide])
        [0..boardSide]
  in
    (Model 0 board True, raiseFromTheDead initiallyAlive boardSide)

aliveNeighboursOf : Cell.Model -> List Cell.Model -> List Cell.Model
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
      (cell.x + 1, cell.y + 1)]
    valid =
      List.filter
        (\(x, y) -> x > 0 && y > 0)
      adjacent
  in
    List.filter
      (\c -> c.alive && (List.member (c.x, c.y) valid))
      cells

updateCell : Cell.Model -> List Cell.Model -> Cell.Model
updateCell cell cells =
  let
    neighbours = aliveNeighboursOf cell cells
  in
    (Cell.update (Cell.Evolve neighbours) cell)

shouldBeBorn cell reborn =
  List.member
    (cell.x, cell.y)
    reborn

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Pause ->
      ({ model | paused = (not model.paused) }, Cmd.none)
    Restart ->
      init
    RaiseFromTheDead reborn ->
      ({ model |
        cells =
          List.map
            (\cell ->
              if (shouldBeBorn cell reborn) then
                { cell | alive = True }
              else cell)
            model.cells,
            paused = False
          }, Cmd.none)
    NextGen _ ->
      ({ model | generation = model.generation + 1
       , cells = List.map (\cell -> (updateCell cell model.cells)) model.cells
       }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  if (model.paused || List.all (\x -> not x.alive) model.cells) then
    Sub.none
  else
    Time.every (250 * millisecond) NextGen

view : Model -> Html Msg
view model =
  div [] [
    span [] [text ("Generation: " ++ (toString model.generation))],
    button [onClick Pause] [text (if (model.paused) then "Resume" else "Pause")],
    button [onClick Restart] [text "Restart"],
    svg [ viewBox "0 0 600 600", height "600px", width "600px" ]
      (List.map Cell.view model.cells)
  ]
