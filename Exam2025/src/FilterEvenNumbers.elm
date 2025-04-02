module FilterEvenNumbers exposing (..)

evens : List Int -> List Int
evens list =
  List.filter (\n -> modBy 2 n == 0 ) list
