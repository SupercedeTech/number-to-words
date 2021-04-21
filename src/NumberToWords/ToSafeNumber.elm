module NumberToWords.ToSafeNumber exposing (toSafeNumber, maxSafeInteger)

import NumberToWords.Error exposing (NTWError(..))

maxSafeInteger : Int
maxSafeInteger = 2 ^ 53 - 1

isSafeNumber : Float -> Bool
isSafeNumber number = Basics.abs (Basics.round number) <= maxSafeInteger

isFinite : Float -> Bool
isFinite n = not (isInfinite n || isNaN n)

toSafeNumber : Float -> Result NTWError Float
toSafeNumber v =
  if not <| isFinite v
  then Err NaNOrInfinty
  else if not <| isSafeNumber v
       then Err TooBig
       else Ok v
