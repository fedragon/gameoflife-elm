module CellTest exposing (tests)

import Cell exposing (Model, Msg(Evolve), update)
import ElmTest exposing (..)

cell x y alive =
  Model x y 0 alive

tests : Test
tests =
    suite "A cell"
        [ test "dies if left alone" (
            let
              original = (cell 0 0 True)
              updated = update (Evolve []) original
            in
              (assertEqual False updated.alive)),
          test "dies if left with just another cell" (
            let
              original = (cell 0 0 True)
              other = (cell 1 1 True)
              updated = update (Evolve [other]) original
            in
              (assertEqual False updated.alive)),
          test "survives with two other cells" (
            let
              original = (cell 0 0 True)
              updated = update (
                Evolve [(cell 1 1 True), (cell 1 0 True)]) original
            in
              (assertEqual True updated.alive)),
          test "reproduces in presence of three other cells" (
            let
              original = (cell 0 0 False)
              updated = update (
                Evolve [
                  (cell 1 1 True),
                  (cell 0 1 True),
                  (cell 1 0 True)]) original
            in
              (assertEqual True updated.alive)),
          test "dies with more than three other cells" (
            let
              original = (cell 1 1 True)
              updated = update (
                Evolve [
                  (cell 2 2 True),
                  (cell 2 1 True),
                  (cell 0 1 True),
                  (cell 1 0 True)]) original
            in
              (assertEqual False updated.alive))]