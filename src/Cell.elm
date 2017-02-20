module Cell exposing (Model, Msg(Evolve, Resurrect), init, update, view)

import Array
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
  }

init x y z =
  Model x y z False 0

type Msg =
  Resurrect
  | Evolve (List Model)

reproduce model = { model | alive = True }

die model = { model | alive = False, age = Basics.max 0 model.age - 1 }

survive model = { model | age = Basics.min 254 model.age + 1 }

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
      (Array.get index yellows)
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
