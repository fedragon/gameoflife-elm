import Board
import Cell

import Array exposing (Array)
import Dict
import Html exposing (Html, button, div, span, text)
import Html.Events exposing (onClick)
import Random
import Svg exposing (svg)
import Svg.Attributes exposing (height, viewBox, width)
import Time exposing (Time, every, millisecond)

type alias Model =
  { generation : Int
  , boardSide : Int
  , board : Board.Model
  , paused : Bool
  }

type Msg =
  NextGen Time
  | Restart
  | Pause
  | GenerateColours (List Int)
  | RaiseFromTheDead (List (Int, Int))

main =
  Html.program {
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

generateColours : Int -> Cmd Msg
generateColours total =
  Random.generate
    GenerateColours
    (Random.list total (Random.int 0 3))

init : (Model, Cmd Msg)
init =
  let
    boardSide = 50
  in
    (Model 0 boardSide Board.empty True, generateColours (boardSide * boardSide))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GenerateColours colours ->
      let
        cellSize = 10
        initiallyAlive = model.boardSide * 4
      in
        ({ model |
          board = Board.init model.boardSide cellSize (Array.fromList colours) },
          raiseFromTheDead initiallyAlive model.boardSide)
    Pause ->
      ({ model | paused = (not model.paused) }, Cmd.none)
    Restart ->
      init
    RaiseFromTheDead these ->
      ({ model |
        board = (Board.makeAlive these model.board),
        paused = False },
        Cmd.none)
    NextGen _ ->
      ({ model | generation = model.generation + 1
       , board = Board.update model.board },
       Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  if (model.paused) then
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
      (List.map Cell.view (Dict.values model.board))
  ]
