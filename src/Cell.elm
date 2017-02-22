module Cell exposing (Model, Msg(Evolve, Resurrect), init, update, view)

import Array exposing (Array)
import Hex
import Maybe
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)

type alias Model =
  { x : Int
  , y : Int
  , z : Int
  , alive : Bool
  , age : Int
  , palette : Array String
  }

init : Int -> Int -> Int -> Int -> Model
init x y z p =
  let
    palette = case p of
      0 -> Array.fromList ["641E16", "7B241C", "922B21", "A93226", "C0392B", "CD6155", "D98880", "E6B0AA"] -- reds
      1 -> Array.fromList ["154360", "1A5276", "1F618D", "2471A3", "2980B9", "5499C7", "7FB3D5", "A9CCE3"] -- blues
      2 -> Array.fromList ["0E6251", "117864", "148F77", "17A589", "1ABC9C", "48C9B0", "76D7C4", "A3E4D7"] -- greens
      _ -> Array.fromList ["7D6608", "9A7D0A", "B7950B", "D4AC0D", "F1C40F", "F4D03F", "F7DC6F", "F9E79F"] -- yellows
  in
    (Model x y z False 0 palette)

type Msg =
  Resurrect
  | Evolve (List Model)

reproduce model = { model | alive = True }

die model = { model | alive = False }

-- just for readability
survive model = model

update : Msg -> Model -> Model
update msg model =
  case msg of
    Resurrect ->
      reproduce model
    Evolve neighbours ->
      case neighbours of
        [] -> die model -- undercrowded
        h::[] -> die model -- undercrowded
        n ->
          case (List.length n) of
            2 -> survive model
            3 -> if (model.alive) then (survive model) else (reproduce model)
            _ -> die model -- overcrowded

increment : Int -> String -> String
increment remainder hex =
  hex
  |> String.toLower
  |> Hex.fromString
  |> Result.map (\n -> Hex.toString (n + remainder))
  |> Result.withDefault hex

colorByAge : Model -> String
colorByAge model =
  if model.age == 0 && model.alive == False then
    "#283747" -- dark blue background
  else
    let
      index = model.age // 10
      remainder = rem model.age 10
    in
      Array.get index model.palette
      |> Maybe.map (increment remainder)
      |> Maybe.withDefault "D6DBDF"
      |> (++) "#"

view : Model -> Svg a
view model =
  let
    color = colorByAge model
  in
    rect
    [ x (model.x * model.z |> toString)
      , y (model.y * model.z |> toString)
      , width (toString model.z)
      , height (toString model.z)
      , fill color
    ] []
