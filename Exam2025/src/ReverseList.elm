module ReverseList exposing (..)

reverseList : List a -> List a
reverseList List =
    case list of
        [] ->
            []
        
        [x] ->
            [x]

        x :: xs ->
            reverseList xs ++ [x]