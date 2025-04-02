module Sort exposing (..)

-- Merge sort algorithm
msort : List comparable -> List comparable
msort list =
    case list of 
        -- Base case: empty list or single-element list is already sorted
        [] -> []
        [x] -> [x]
        _ ->
            -- Recursively split the list into halves and merge them
            let
                (left, right) = split list
            in
            merge (msort left) (msort right)

-- Merge two sorted lists
merge : List comparable -> List comparable -> List comparable
merge list1 list2 =
    case (list1, list2) of
        -- If either list is empty, return the other list
        ([], _) -> list2
        (_, []) -> list1
        -- If both lists have elements, compare the heads and merge accordingly
        (x :: xs, y :: ys) ->
            if x <= y then
                x :: merge xs (y :: ys)
            else
                y :: merge (x :: xs) ys

-- Split a list into two halves
split : List comparable -> ( List comparable, List comparable )
split list =
    let
        len = List.length list
        half = len // 2
    in
    (List.take half list, List.drop half list)



runTests : List Bool
runTests =       
    let
        tests =
            [
                ("Test sorting an empty list", msort [] == []),
                ("Test sorting a list with one element", msort [5] == [5]),
                ("Test sorting a list with multiple elements", msort [5, 2, 8, 1, 9, 3] == [1, 2, 3, 5, 8, 9])
            ]
    in
    List.map (\(desc, result) -> Debug.log desc result) (List.reverse tests)
