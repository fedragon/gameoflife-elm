import ElmTest exposing (..)

import BoardTest
import CellTest

main =
  runSuiteHtml
    (suite "All together now" [CellTest.tests, BoardTest.tests])