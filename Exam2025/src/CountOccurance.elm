module CountOccurance exposing (..)


countOccurrence : comparable -> List comparable -> Int
countOccurrence com list =
    List.foldl (\x acc -> if x == com then acc + 1 else acc) 0 list
