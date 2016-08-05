module NeighboursTest exposing (tests)

import Cell exposing (Model)
import GoL exposing (aliveNeighboursOf)
import ElmTest exposing (..)

tests : Test
tests =
  suite "When determining neighbours of a cell" [
    test "only alive neighbours are taken into account" (
      let
        cell = (Model 0 0 0 True)
        theOne = (Model 0 1 0 True)
        cells = [theOne, (Model 1 0 0 False)]
      in
        (assertEqual [theOne] (aliveNeighboursOf cell cells))
    ),
    test "only adjacent neighbours are taken into account" (
      let
        cell = (Model 0 0 0 True)
        theOne = (Model 0 1 0 True)
        cells = [theOne, (Model 0 2 0 True)]
      in
        (assertEqual [theOne] (aliveNeighboursOf cell cells))
    ),
    test "negative coordinates are discarded" (
      let
        cell = (Model 0 0 0 True)
        theOne = (Model 0 1 0 True)
        cells = [theOne, (Model 0 -1 0 True)]
      in
        (assertEqual [theOne] (aliveNeighboursOf cell cells))
    )

  ]