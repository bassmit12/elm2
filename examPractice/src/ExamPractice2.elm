module ExamPractice2 exposing (..)
import Html exposing (b)

filterEven : List Int -> List Int
filterEven list =
    List.filter (\x -> modBy 2 x == 0) list

sumOfSquares : List Int -> Int
sumOfSquares list = 
    List.sum (List.map (\x -> x * x) list)

insert : comparable -> List comparable -> List comparable
insert value list =
    case list of
        [] ->
            [value]

        x :: xs ->
            if x > value then
                value :: list
            else
                x :: insert value xs
        
insertionSort: List comparable -> List comparable -> List comparable
insertionSort list1 list2 =
    case list1 of 
        [] -> list2

        y :: ys ->
            insertionSort ys (insert y list2)

posProductRec: List (Int, Int) -> List Int
posProductRec list =
    case list of 
        [] -> []

        (a, b) :: rest ->
            let
                product = a * b
                restProducts = posProductRec rest
            in 

            if product > 0 then
                product :: restProducts 
            else 
                restProducts

posProduct : List (Int, Int) -> List Int
posProduct tuples =
    tuples
        |> List.filter (\(a, b) -> a * b > 0) -- Filter tuples where the product is positive
        |> List.map (\(a, b) -> a * b) -- Map the filtered tuples to their product


layoutB: List Int -> String
layoutB = List.foldl accLeft initLeft

-- step
accLeft : a -> b -> String
accLeft number operation = 
    "/" ++ String.fromInt number

-- begin state
initLeft : String
initLeft = ""