module NumberToWords.ToWordsOrdinal exposing (toWordsOrdinal)

{-| This module contains a function that allows you to get ordinal number words from numbers

@docs toWordsOrdinal

-}

import NumberToWords.MakeOrdinal exposing (makeOrdinal)
import NumberToWords.ToWords exposing (toWords)
import NumberToWords.Error exposing (NTWError)

{-| Convert number into ordinal number words.

    toWordsOrdinal 0    == Ok "zeroth"
    toWordsOrdinal 1.9  == Ok "second"
    toWordsOrdinal 1000 == Ok "one thousandth"
    toWordsOrdinal NaN  == Err NaNOrInfinty
    toWordsOrdinal -10  == Ok "minus tenth"
-}
toWordsOrdinal : Float -> Result NTWError String
toWordsOrdinal number =
  let words = toWords number
  in Result.map makeOrdinal words
