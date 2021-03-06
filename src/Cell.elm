module Cell exposing (Model, Msg(..), init, update, view)

import Debug exposing (toString)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Model =
    { x : Int
    , y : Int
    , z : Int
    , alive : Bool
    }


init x y z =
    Model x y z False


type Msg
    = Resurrect
    | Evolve (List Model)


reproduce model =
    { model | alive = True }


die model =
    { model | alive = False }



-- just for readability


survive model =
    model


update : Msg -> Model -> Model
update msg model =
    case msg of
        Resurrect ->
            reproduce model

        Evolve neighbours ->
            case neighbours of
                [] ->
                    die model

                -- undercrowded
                h :: [] ->
                    die model

                -- undercrowded
                n ->
                    case List.length n of
                        2 ->
                            survive model

                        3 ->
                            if model.alive then
                                survive model

                            else
                                reproduce model

                        _ ->
                            die model



-- overcrowded


view : Model -> Svg a
view model =
    let
        color =
            if model.alive then
                "black"

            else
                "white"
    in
    rect
        [ x (model.x * model.z |> toString)
        , y (model.y * model.z |> toString)
        , width (toString model.z)
        , height (toString model.z)
        , fill color
        , stroke "black"
        , strokeWidth "1px"
        ]
        []
