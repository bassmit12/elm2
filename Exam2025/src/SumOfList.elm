module SumOfList exposing (..)

sumList : List Int -> Int
sumList list = 
    case list of
        [] -> 0
        [x] -> x 
        x :: xs ->
            x + sumList xs
        