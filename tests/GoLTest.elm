module Main exposing (main)

import BoardTest
import CellTest
import ElmTest exposing (..)


main =
    runSuiteHtml
        (suite "All together now" [ CellTest.tests, BoardTest.tests ])
