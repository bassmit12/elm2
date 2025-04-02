module UniqueElements exposing (..)

unique : List comparable -> List comparable
unique list =
    List.reverse (List.foldl (\x acc -> if List.member x acc then acc else x :: acc) [] list)
