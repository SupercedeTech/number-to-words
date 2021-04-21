module NumberToWords.ToWords exposing (toWords)

{-| This module contains a function that allows you to get words from numbers.

@docs toWords

-}

import NumberToWords.ToSafeNumber exposing (maxSafeInteger, toSafeNumber)
import NumberToWords.MakeOrdinal exposing (makeOrdinal)
import NumberToWords.Error exposing (NTWError(..))

import Array exposing (Array)

import Regex exposing (Regex)

ten : Int
ten = 10

oneHundred : Int
oneHundred = 100

oneThousand : Int
oneThousand = 1000

oneMillion : Int
oneMillion = 1000000

oneBillion : Int
oneBillion = 1000000000           -- 1.000.000.000 (9)

oneTrillion : Int
oneTrillion = 1000000000000       --  1.000.000.000.000 (12)

oneQuadrillion : Int
oneQuadrillion = 1000000000000000 -- 1.000.000.000.000.000 (15)

max : Int
max = maxSafeInteger              -- 9.007.199.254.740.992 (15)

lessThanTwenty : Array String
lessThanTwenty = Array.fromList [
    "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
    "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"
  ]

tenthsLessThanHundred : Array String
tenthsLessThanHundred = Array.fromList [
    "zero", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"
  ]

{-| Convert number into words.

    toWords 0    == Ok "zero"
    toWords 1.9  == Ok "two"
    toWords 2222 == Ok "two thousand, two hundred twenty-two"
    toWords NaN  == Err NaNOrInfinty
    toWords -19   == Ok "minus nineteen"
-}
toWords : Float -> Result NTWError String
toWords number = toSafeNumber number |>
  Result.andThen (\v -> generateWords (Basics.round v) [])

generateWords : Int -> List String -> Result NTWError String
generateWords number words =
  if number == 0 -- We're done
  then
    if List.isEmpty words
    then Ok "zero"
    else Ok (String.join " " words
      |> String.replace " - " "-"
      |> Regex.replace endByComma (\_ -> ""))
  else
    -- If negative, prepend "minus"
    if (number < 0)
    then generateWords (Basics.abs number) ["minus"]
    else
      if number < 20
      then generateWords 0 (List.append words [Array.get number lessThanTwenty |> Maybe.withDefault ""])
      else
        if number < oneHundred
        then
          let
            remainder = Basics.remainderBy ten number
            word = Array.get (number // ten) tenthsLessThanHundred
          in
            if remainder > 0
            then generateWords 0 (
              List.concat
                [ words
                , [ word |> Maybe.withDefault "" ]
                , [ Array.get remainder lessThanTwenty |> Maybe.map (\v -> "- " ++ v) |> Maybe.withDefault "" ]
                ]
              )
            else generateWords 0 (List.append words [word |> Maybe.withDefault ""])
        else
          let genPart = generatePart number words
          in
          if number < oneThousand
          then genPart oneHundred " hundred"
          else
            if number < oneMillion
            then genPart oneThousand " thousand,"
            else
              if number < oneBillion
              then genPart oneMillion " million,"
              else
                if number < oneTrillion
                then genPart oneBillion " billion,"
                else
                  if number < oneQuadrillion
                  then genPart oneTrillion " trillion,"
                  else
                    if number <= max
                    then genPart oneQuadrillion " quadrillion,"
                    else Err TooBig

generatePart : Int -> List String -> Int -> String -> Result NTWError String
generatePart number words factor name =
  let word = generateWords (Basics.floor <| Basics.toFloat number / Basics.toFloat factor) []
                |> Result.map (\w -> w ++ name)
  in
    Result.andThen
      (\v -> generateWords
        (Basics.remainderBy factor number)
        (List.append words [ v ])
      ) word

endByComma : Regex
endByComma =
  Maybe.withDefault Regex.never <|
  Regex.fromString ",$"
