module ToWordsOrdinalSpec exposing (..)

import Expect
import Test exposing (Test, describe, test, skip)

import NumberToWords.ToWordsOrdinal exposing (toWordsOrdinal)
import NumberToWords.ToSafeNumber exposing (maxSafeInteger)
import NumberToWords.Error exposing (NTWError(..))

import Util exposing (print)

suite : Test
suite =
    describe "To Words Ordinal" <|
        (List.map
          (\(number, ordinal) ->
            test (print number ordinal) <| \_ -> toWordsOrdinal number |> Expect.equal (Ok ordinal)
          )
          data
        ) ++
          [ test "Return TooBig when input more than maxSafeInt"
              <| \_ -> toWordsOrdinal (Basics.toFloat <| maxSafeInteger + 1) |> Expect.equal (Err TooBig)
          , test "Return TooBig when input less than -maxSafeInt"
              <| \_ -> toWordsOrdinal (Basics.toFloat <| -1 * (maxSafeInteger + 1)) |> Expect.equal (Err TooBig)
          , test "Return NaNOrInfinty when input is NaN"
              <| \_ -> toWordsOrdinal (0/0) |> Expect.equal (Err NaNOrInfinty)
          ]


data : List (Float, String)
data = [
    (0, "zeroth")
  , (1, "first")
  , (2, "second")
  , (3, "third")
  , (4, "fourth")
  , (5, "fifth")
  , (6, "sixth")
  , (7, "seventh")
  , (8, "eighth")
  , (9, "ninth")
  , (10, "tenth")
  , (11, "eleventh")
  , (12, "twelfth")
  , (13, "thirteenth")
  , (14, "fourteenth")
  , (15, "fifteenth")
  , (16, "sixteenth")
  , (17, "seventeenth")
  , (18, "eighteenth")
  , (19, "nineteenth")
  , (20, "twentieth")
  , (21, "twenty-first")
  , (22, "twenty-second")
  , (23, "twenty-third")
  , (24, "twenty-fourth")
  , (25, "twenty-fifth")
  , (26, "twenty-sixth")
  , (27, "twenty-seventh")
  , (28, "twenty-eighth")
  , (29, "twenty-ninth")
  , (30, "thirtieth")
  , (40, "fortieth")
  , (50, "fiftieth")
  , (60, "sixtieth")
  , (70, "seventieth")
  , (80, "eightieth")
  , (90, "ninetieth")
  , (100, "one hundredth")
  , (1000, "one thousandth")
  , (1000000, "one millionth")
  , (1000000000, "one billionth")
  , (1000000000000, "one trillionth")
  , (1000000000000000, "one quadrillionth")
  ]
