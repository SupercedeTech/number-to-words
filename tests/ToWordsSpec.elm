module ToWordsSpec exposing (..)

import Expect
import Test exposing (Test, describe, test, skip, fuzz)
import Fuzz as F

import NumberToWords.ToSafeNumber exposing (maxSafeInteger)
import NumberToWords.ToWords exposing (toWords)
import NumberToWords.Error exposing (NTWError(..))

import Util exposing (print)

suite : Test
suite =
  describe "To Words" <|
      (List.map
        (\(number, words) -> test (print number words) <| \_ -> toWords number |> Expect.equal (Ok words))
        data
      )  ++
          [ test "Return TooBig when input more than maxSafeInt"
              <| \_ -> toWords (Basics.toFloat <| maxSafeInteger + 1) |> Expect.equal (Err TooBig)
          , test "Return TooBig when input less than -maxSafeInt"
              <| \_ -> toWords (Basics.toFloat <| -1 * (maxSafeInteger + 1)) |> Expect.equal (Err TooBig)
          , test "Return NaNOrInfinty when input is NaN"
              <| \_ -> toWords (0/0) |> Expect.equal (Err NaNOrInfinty)
          , fuzz (F.map Basics.toFloat <| F.intRange 1000000 maxSafeInteger) "Should contains 'illion'"
              <| \fuzzValue ->
                  toWords fuzzValue
                    |> Result.toMaybe
                    |> Maybe.map (String.contains "illion")
                    |> Expect.equal (Just True)
          ]

data : List (Float, String)
data = [
    (-1, "minus one")
  , (0, "zero")
  , (1, "one")
  , (1.9, "two")
  , (2, "two")
  , (3, "three")
  , (4, "four")
  , (5, "five")
  , (6, "six")
  , (7, "seven")
  , (8, "eight")
  , (9, "nine")
  , (10, "ten")
  , (11, "eleven")
  , (12, "twelve")
  , (13, "thirteen")
  , (14, "fourteen")
  , (15, "fifteen")
  , (16, "sixteen")
  , (17, "seventeen")
  , (18, "eighteen")
  , (19, "nineteen")
  , (20, "twenty")
  , (22, "twenty-two")
  , (33, "thirty-three")
  , (44, "forty-four")
  , (55, "fifty-five")
  , (66, "sixty-six")
  , (77, "seventy-seven")
  , (88, "eighty-eight")
  , (99, "ninety-nine")
  , (100, "one hundred")
  , (111, "one hundred eleven")
  , (1000, "one thousand")
  , (2222, "two thousand, two hundred twenty-two")
  , (10000, "ten thousand")
  , (33333, "thirty-three thousand, three hundred thirty-three")
  , (100000, "one hundred thousand")
  , (444444, "four hundred forty-four thousand, four hundred forty-four")
  , (1000000, "one million")
  , (5555555, "five million, five hundred fifty-five thousand, five hundred fifty-five")
  , (10000000, "ten million")
  , (66666666, "sixty-six million, six hundred sixty-six thousand, six hundred sixty-six")
  , (100000000, "one hundred million")
  , (777777777, "seven hundred seventy-seven million, seven hundred seventy-seven thousand, seven hundred seventy-seven")
  , (1000000000, "one billion")
  , (8888888888, "eight billion, eight hundred eighty-eight million, eight hundred eighty-eight thousand, eight hundred eighty-eight")
  , (10000000000, "ten billion")
  , (99999999999, "ninety-nine billion, nine hundred ninety-nine million, nine hundred ninety-nine thousand, nine hundred ninety-nine")
  , (100000000000, "one hundred billion")
  , (111111111111, "one hundred eleven billion, one hundred eleven million, one hundred eleven thousand, one hundred eleven")
  , (1000000000000, "one trillion")
  , (2222222222222, "two trillion, two hundred twenty-two billion, two hundred twenty-two million, two hundred twenty-two thousand, two hundred twenty-two")
  , (10000000000000, "ten trillion")
  , (33333333333333, "thirty-three trillion, three hundred thirty-three billion, three hundred thirty-three million, three hundred thirty-three thousand, three hundred thirty-three")
  , (100000000000000, "one hundred trillion")
  , (444444444444444, "four hundred forty-four trillion, four hundred forty-four billion, four hundred forty-four million, four hundred forty-four thousand, four hundred forty-four")
  , (1000000000000000, "one quadrillion")
  , (5555555555555555, "five quadrillion, five hundred fifty-five trillion, five hundred fifty-five billion, five hundred fifty-five million, five hundred fifty-five thousand, five hundred fifty-five")
  ]
