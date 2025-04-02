module ProductOfList exposing (..)

product : List Int ->  Int
product list =
  List.foldl (\x acc -> x * acc ) 1 list
