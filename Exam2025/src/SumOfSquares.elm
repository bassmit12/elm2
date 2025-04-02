module SumOfSquares exposing(..)

sumOfSquares: List Int -> Int
sumOfSquares ints =
  List.sum (List.map (\n -> n * n) ints)
