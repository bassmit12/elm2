module DoubleEvens exposing (..)

doubleEvens : List Int -> List Int
doubleEvens list =
  List.map ((*) 2) (List.filter (\x -> modBy 2 x == 0) list)
