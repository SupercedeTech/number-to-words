module Util exposing (print)

print : Float -> String -> String
print i o = String.fromFloat i ++ " -> " ++ o
