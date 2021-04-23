module UtilSpec exposing (..)

import Expect
import Fuzz as F
import Test exposing (Test, describe, test, skip, fuzz, only)
import NumberToWords.Util exposing (cond)

suite : Test
suite =
  describe "The Util module"
    [ describe "cond"
      [ test "Should return third option number 3" <|
          \_ ->
            let options =
                  [ (Basics.always False, Basics.always 1)
                  , (Basics.always False, Basics.always 2)
                  , (Basics.always True, Basics.always 3)
                  , (Basics.always False, Basics.always 4)
                  ]
            in Expect.equal (cond 0 True options) 3
      , test "Returns only the first True encountered" <|
          \_ ->
            let options =
                  [ (Basics.always True, Basics.always 1)
                  , (Basics.always True, Basics.always 2)
                  , (Basics.always True, Basics.always 3)
                  , (Basics.always True, Basics.always 4)
                  ]
            in Expect.equal (cond 0 True options) 1
      , test "Returns the default value when everything is False" <|
          \_ ->
            let options =
                  [ (Basics.always False, Basics.always 1)
                  , (Basics.always False, Basics.always 2)
                  , (Basics.always False, Basics.always 3)
                  , (Basics.always False, Basics.always 4)
                  ]
            in Expect.equal (cond 0 True options) 0
      ]
    ]
