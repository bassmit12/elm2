module SumList exposing (..)

sumList: List Int -> Int 
sumList list =
  case list of
    [] -> 0
    
    x::xs ->
      x + sumList xs
