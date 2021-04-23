module NumberToWords.Util exposing (cond)

cond : r -> a -> List (a -> Bool, a -> r) -> r
cond default candidate pairs =
  List.map (Tuple.mapFirst (\condition -> condition candidate)) pairs
    |> List.filter (((==) True) << Tuple.first)
    |> ((Maybe.map Tuple.second) << List.head)
    |> Maybe.map (\f -> f candidate)
    |> Maybe.withDefault default
