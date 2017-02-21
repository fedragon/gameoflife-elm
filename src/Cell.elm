module Cell exposing (Model, Msg(Evolve, Resurrect), init, update, view)

import Array exposing (Array)
import Hex
import Maybe
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

init x y z =
  Model x y z False 0 Array.empty

type Msg =
  Resurrect Int
  | Evolve (List Model)

resurrect model color =
  let
    palette = case color of
      0 -> greens
      1 -> yellows
      2 -> reds
      _ -> blues
  in
    { model | alive = True, palette = palette }

reproduce model = { model | alive = True }

die model = { model | alive = False, age = Basics.max 0 model.age - 1 }

survive model = { model | age = Basics.min 254 model.age + 1 }

update : Msg -> Model -> Model
update msg model =
  case msg of
    Resurrect color ->
      resurrect model color
    Evolve neighbours ->
      case neighbours of
        [] -> die model -- undercrowded
        h::[] -> die model -- undercrowded
        n ->
          case (List.length n) of
            2 -> survive model
            3 -> if (model.alive) then (survive model) else (reproduce model)
            _ -> die model -- overcrowded

greens =
  Array.fromList [
    "0E6251",
    "117864",
    "148F77",
    "17A589",
    "1ABC9C",
    "48C9B0",
    "76D7C4",
    "A3E4D7"
  ]

blues =
  Array.fromList [
    "154360",
    "1A5276",
    "1F618D",
    "2471A3",
    "2980B9",
    "5499C7",
    "7FB3D5",
    "A9CCE3"
  ]

reds =
  Array.fromList [
    "641E16",
    "7B241C",
    "922B21",
    "A93226",
    "C0392B",
    "CD6155",
    "D98880",
    "E6B0AA"
  ]

yellows =
  Array.fromList [
    "7D6608",
    "9A7D0A",
    "B7950B",
    "D4AC0D",
    "F1C40F",
    "F4D03F",
    "F7DC6F",
    "F9E79F"
  ]

increment : Int -> String -> String
increment remainder hex =
  hex
  |> String.toLower
  |> Hex.fromString
  |> Result.map (\n -> Hex.toString (n + remainder))
  |> Result.withDefault hex

colorByAge : Model -> String
colorByAge model =
  if model.age < 2 then
    -- dark blue background
    "#283747"
  else
    let
      index = model.age // 10
      remainder = rem model.age 10
    in
      (Array.get index model.palette)
      |> Maybe.map (increment remainder)
      |> Maybe.withDefault "D6DBDF"
      |> (++) "#"

view : Model -> Svg a
view model =
  rect
  [ x (model.x * model.z |> toString)
    , y (model.y * model.z |> toString)
    , width (toString model.z)
    , height (toString model.z)
    , fill (colorByAge model)
  ] []
