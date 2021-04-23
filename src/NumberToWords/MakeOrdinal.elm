module NumberToWords.MakeOrdinal exposing (makeOrdinal)

import Dict exposing (Dict)
import Regex exposing (Regex)
import NumberToWords.Util exposing (cond)

mkRegex : String -> Regex
mkRegex str = Maybe.withDefault Regex.never <|
  Regex.fromString str

endsWithDoubleZeroPattern : Regex
endsWithDoubleZeroPattern = mkRegex "(hundred|thousand|(m|b|tr|quadr)illion)$"

endsWithTeenPattern : Regex
endsWithTeenPattern = mkRegex "teen$"

endsWithYPattern : Regex
endsWithYPattern = mkRegex "y$"

endsWithZeroThroughTwelvePattern : Regex
endsWithZeroThroughTwelvePattern = mkRegex "(zero|one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve)$"

ordinalLessThanThirteen : Dict String String
ordinalLessThanThirteen =
  Dict.fromList [
    ("zero", "zeroth")
  , ("one", "first")
  , ("two", "second")
  , ("three", "third")
  , ("four", "fourth")
  , ("five", "fifth")
  , ("six", "sixth")
  , ("seven", "seventh")
  , ("eight", "eighth")
  , ("nine", "ninth")
  , ("ten", "tenth")
  , ("eleven", "eleventh")
  , ("twelve", "twelfth")
  ]


makeOrdinal : String -> String
makeOrdinal words =
  cond words words
    [ -- Ends with *00 (100, 1000, etc.) or *teen (13, 14, 15, 16, 17, 18, 19)
     (\w -> (Regex.contains endsWithDoubleZeroPattern w) || (Regex.contains endsWithTeenPattern w), \w -> w ++ "th")
    , -- Ends with *y (20, 30, 40, 50, 60, 70, 80, 90)
     (\w -> Regex.contains endsWithYPattern w, \w -> Regex.replace endsWithYPattern (\_ -> "ieth") w)
    , -- Ends with one through twelve
     (\w -> Regex.contains endsWithZeroThroughTwelvePattern w
     , \w -> Regex.replace endsWithZeroThroughTwelvePattern replaceWithOrdinalVariant w
     )
    ]


replaceWithOrdinalVariant : Regex.Match -> String
replaceWithOrdinalVariant ({ match }) =
  Maybe.withDefault match <|
    Dict.get match ordinalLessThanThirteen
