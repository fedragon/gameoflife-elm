import ElmTest exposing (..)

import NeighboursTest
import CellTest

main =
  runSuiteHtml
    (suite "All together now" [CellTest.tests, NeighboursTest.tests])