module ToOrdinalSpec exposing (..)

import Expect
import Test exposing (Test, describe, test, fuzz)
import Fuzz as F

import NumberToWords.ToSafeNumber exposing (maxSafeInteger)
import NumberToWords.ToOrdinal exposing (toOrdinal)
import NumberToWords.Error exposing (NTWError(..))

import Util exposing (print)

suite : Test
suite =
  describe "To Ordinal" <|
      (List.map
        (\(number, ordinal) -> test (print number ordinal) <| \_ -> toOrdinal number |> Expect.equal (Ok ordinal))
        data
      ) ++
          [ test "Return NaNOrInfinty when input is NaN"
              <| \_ -> toOrdinal (0/0) |> Expect.equal (Err NaNOrInfinty)
          , fuzz (F.map Basics.toFloat <| F.intRange (-maxSafeInteger) maxSafeInteger) "Last 2 chars shouldn't be digits"
              <| \fuzzValue ->
                  toOrdinal fuzzValue
                  |> Result.map (\v -> String.dropLeft ((String.length v) - 2) v)
                  |> Result.map (List.all Char.isAlpha << String.toList)
                  |> Expect.equal (Ok True)
          ]

data : List (Float, String)
data = [
    (-121, "-121st")
  , (-13, "-13th")
  , (-12, "-12th")
  , (-11, "-11th")
  , (-3, "-3rd")
  , (-2, "-2nd")
  , (-1, "-1st")
  , (0, "0th")
  , (1, "1st")
  , (1.9, "2nd")
  , (2, "2nd")
  , (3, "3rd")
  , (4, "4th")
  , (5, "5th")
  , (6, "6th")
  , (7, "7th")
  , (8, "8th")
  , (9, "9th")
  , (10, "10th")
  , (11, "11th")
  , (12, "12th")
  , (13, "13th")
  , (121, "121st")
  ]
