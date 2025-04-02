module AverageOfList exposing (..)

average : List Float -> Float
average list =
  let
    total = List.sum list
    length = List.length list
  in

  total / (toFloat length)
