module NumberToWords.ToOrdinal exposing (toOrdinal)

{-| This module contains a function that allows you to get ordinal from numbers.

@docs toOrdinal

-}

import NumberToWords.ToSafeNumber exposing (toSafeNumber)
import NumberToWords.Error exposing (NTWError(..))

{-| Convert number into ordinal numbers.

    toOrdinal 0   == Ok "0th"
    toOrdinal 10  == Ok "10th"
    toOrdinal -10 == Ok "-10th"
    toOrdinal NaN == Err NaNOrInfinty
    toOrdinal 0.7 == OK "1st"
-}
toOrdinal : Float -> Result NTWError String
toOrdinal number =
  toSafeNumber number |>
    Result.map (\v ->
      let
        str = String.fromInt (Basics.round v)
        lastTwoDigits = Basics.round v
          |> Basics.remainderBy 100
          |> Basics.abs
        betweenElevenAndThirteen = lastTwoDigits >= 11 && lastTwoDigits <= 13
        lastChar = String.reverse str |> String.toList |> List.head
      in
        str ++ (if betweenElevenAndThirteen then "th"
            else
              if lastChar == Just '1' then "st"
            else
              if lastChar == Just '2' then "nd"
            else
              if lastChar == Just '3' then "rd"
            else "th"
          )
    )
