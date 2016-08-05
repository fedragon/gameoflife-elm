module BoardTest exposing(tests)

import ElmTest exposing (..)

import Board exposing (..)
import Cell

import Dict exposing (fromList)

tests : Test
tests =
  suite "When determining neighbours of a cell" [
    test "only alive neighbours are taken into account" (
      let
        cell = (Cell.Model 0 0 0 True)
        theOne = (Cell.Model 0 1 0 True)
        cells = fromList [((0, 1), theOne), ((1, 0), (Cell.Model 1 0 0 False))]
      in
        (assertEqual [theOne] (aliveNeighboursOf cell cells))
    ),
    test "only adjacent neighbours are taken into account" (
      let
        cell = (Cell.Model 0 0 0 True)
        theOne = (Cell.Model 0 1 0 True)
        cells = fromList [((0, 1), theOne), ((0, 2), (Cell.Model 0 2 0 True))]
      in
        (assertEqual [theOne] (aliveNeighboursOf cell cells))
    )
  ]