module LengthList exposing (..)

lengthList : List a -> Int
lengthList list = 
    case list of 
        [] -> 0
        x :: xs -> 
            1 + lengthList xs