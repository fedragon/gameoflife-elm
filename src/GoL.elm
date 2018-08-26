module Main exposing (Model, Msg(..), init, main, raiseFromTheDead, subscriptions, update, view)

import Board
import Browser
import Cell
import Debug exposing (toString)
import Dict
import Html exposing (Html, button, div, span, text)
import Html.Events exposing (onClick)
import Random
import Svg exposing (svg)
import Svg.Attributes exposing (height, viewBox, width)
import Time exposing (every)


type alias Model =
    { generation : Int
    , board : Board.Model
    , paused : Bool
    }


type Msg
    = NextGen Time.Posix
    | Restart
    | Pause
    | RaiseFromTheDead (List ( Int, Int ))


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


raiseFromTheDead : Int -> Int -> Cmd Msg
raiseFromTheDead total boardSide =
    Random.generate
        RaiseFromTheDead
        (Random.list
            total
            (Random.pair
                (Random.int 1 boardSide)
                (Random.int 1 boardSide)
            )
        )


init : () -> ( Model, Cmd Msg )
init _ =
    let
        boardSide =
            50

        cellSize =
            10

        initiallyAlive =
            boardSide * 3

        board =
            Board.init boardSide cellSize
    in
    ( Model 1 board False, raiseFromTheDead initiallyAlive boardSide )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pause ->
            ( { model | paused = not model.paused }, Cmd.none )

        Restart ->
            init ()

        RaiseFromTheDead these ->
            ( { model
                | board = Board.makeAlive these model.board
                , paused = False
              }
            , Cmd.none
            )

        NextGen _ ->
            ( { model
                | generation = model.generation + 1
                , board = Board.update model.board
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused then
        Sub.none

    else
        Time.every 250 NextGen


view : Model -> Browser.Document Msg
view model =
    Browser.Document
        "Game of Elm"
        [ div []
            [ span [] [ text ("Generation: " ++ toString model.generation) ]
            , button [ onClick Pause ]
                [ text
                    (if model.paused then
                        "Resume"

                     else
                        "Pause"
                    )
                ]
            , button [ onClick Restart ] [ text "Restart" ]
            , svg [ viewBox "0 0 600 600", height "600px", width "600px" ]
                (List.map Cell.view (Dict.values model.board))
            ]
        ]
