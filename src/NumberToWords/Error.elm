module NumberToWords.Error exposing (NTWError(..))

{-| This module contains a type that allows you to determine the type of error that may occur during conversion.

# Type and Constructors
@docs NTWError

-}

{-| A `NTWError` is either `NaNOrInfinty` meaning the computation failed because user pass `NaN` or `Infinite`, or it is an
`TooBig` meaning that user pass number that more than max safe int.
-}
type NTWError = NaNOrInfinty | TooBig
