module ExamPractice exposing (..)
import Html exposing (a)
import Browser exposing (element)

factorial : Int -> Int
factorial x =
  if x == 0 then
        1
  else
    x * factorial(x - 1)


fibonacci : Int -> Int
fibonacci n =
    if n <= 1 then
        n  -- Base case: Fibonacci of 0 is 0, Fibonacci of 1 is 1
    else
        fibonacci (n - 1) + fibonacci (n - 2)  -- Recursive case: Fibonacci of n is the sum of Fibonacci of (n - 1) and Fibonacci of (n - 2)

reverseList : List Int -> List Int
reverseList list =
  case list of
    [] -> []
    [x] -> [x]

    x :: xs ->
      reverseList xs ++ [x]


sumOfDigits : Int -> Int
sumOfDigits n =
    if n < 10 then
        n  -- Base case: if n is a single digit, return n
    else
        let
            lastDigit = modBy 10 n  -- Extract the last digit of n
            remainingDigits = n // 10  -- Remove the last digit from n
        in
        lastDigit + sumOfDigits remainingDigits  -- Recursive case: add the last digit to the sum of the remaining digits

lengthOfListRec : List Int -> Int
lengthOfListRec list = 
  case list of 
    [] -> 0

    x :: xs ->
      x // x + lengthOfListRec xs

maxElementInList : List Int -> Int 
maxElementInList list = 
  case list of 
    [] -> 0

    x :: xs ->
      if x > maxElementInList xs then
        x 
      else 
        maxElementInList xs

isListSorted : List Int -> Bool
isListSorted list =
  case list of 
    [] -> True
    [x] -> True

    x :: y :: xs ->
      if x <= y then
        isListSorted (y::xs)
      else
        False


power : Int -> Int -> Int
power base exponent =
    if exponent == 0 then
        1  -- Base case: any number raised to the power of 0 is 1
    else if exponent == 1 then
        base  -- Base case: any number raised to the power of 1 is itself
    else
        base * power base (exponent - 1)  -- Recursive case: multiply base by base raised to (exponent - 1)
    

countElementInList : List Int -> Int -> Int
countElementInList list element =
    case list of 
        [] ->
            0  -- Base case: empty list, return 0 occurrences of the element
        
        x :: xs ->
            let
                elementCount = if x == element then 1 else 0  -- Check if the current element matches the desired element
            in 
            elementCount + countElementInList xs element  -- Recursively count occurrences of the element in the rest of the list

onlyOneDiffers : List a -> List a -> Bool
onlyOneDiffers list1 list2 =
    case (list1, list2) of
        ([], []) ->
            False

        ([], _::_) ->
            True

        (_::_, []) ->
            True

        (x::xs, y::ys) ->
            if x /= y then
                onlyOneDiffersHelper xs ys 1
            else
                onlyOneDiffers xs ys


onlyOneDiffersHelper : List a -> List a -> Int -> Bool
onlyOneDiffersHelper list1 list2 count =
    case (list1, list2) of
        ([], []) ->
            count == 1

        ([], _::_) ->
            count == 1

        (_::_, []) ->
            count == 1

        (x::xs, y::ys) ->
            if x /= y then
                onlyOneDiffersHelper xs ys (count + 1)
            else
                onlyOneDiffersHelper xs ys count

firstElem  : List a -> Maybe a
firstElem list1 =
    case list1 of
        [] -> Nothing
        x :: _ -> Just x

firstTwoElems : List a -> Maybe (a, a)
firstTwoElems list =
    case list of
        x :: y :: _ -> Just (x, y)
        _ -> Nothing

isShorterThen : List a -> Int -> Bool 
isShorterThen list1 amount =
    case list1 of
        [] -> amount > 0
        _ :: xs ->
            if amount <= 0 then
                False 
            else 
                isShorterThen xs (amount - 1)

-- elemtAt 3 ['a', 'e', 'i', 'o', 'u']
-- 3 ['a', 'e', 'i', 'o', 'u']
-- 2 ['e', 'i', 'o', 'u']
-- 1 ['i', 'o', 'u']
-- 0 ['o', 'u']

elemAt: Int -> List a -> Maybe a 
elemAt index list =
    if index < 0 then
        Nothing
    else
        case list of 
            [] -> Nothing
            x :: xs ->
                if index == 0 then
                    Just x
                else 
                    elemAt (index - 1) xs

sumList: List Int -> Int
sumList list =
    case list of
        [] -> 0
        [x] -> x 

        x :: xs ->
            x + sumList xs

-- grades = [ ("Anna", 8), ("Jane", 6), ("Jane", 7), ("Paul", 6), ("Anna", 7), ("Jane", 9) ]
report : List (String, Int) -> String -> Maybe (String, List Int, Float)
report grades studentName =
    let
        -- Extract grades for the specified student
        studentGrades =
            List.filter (\(name, _) -> name == studentName) grades
                |> List.map Tuple.second

        -- Calculate the average grade
        averageGrade : List Int -> Float
        averageGrade gradesList =
            if List.isEmpty gradesList then
                0
            else
                List.sum (List.map toFloat gradesList) / toFloat (List.length gradesList)
    in
    if List.isEmpty studentGrades then
        Nothing
    else
        Just (studentName, studentGrades, averageGrade studentGrades)

lastA : List a -> Maybe a 
lastA list =
    case list of 
        [] -> Nothing
        [x] -> Just x 
        x :: xs ->
            lastA xs

-- Implementation of lastB using List.foldl
lastB : List a -> Maybe a
lastB list =
    List.foldl accLeft initLeft list

accLeft : a -> Maybe a -> Maybe a
accLeft x _ = Just x

initLeft : Maybe a
initLeft = Nothing

-- Implementation of lastC using List.foldr
lastC : List a -> Maybe a
lastC list =
    List.foldr accRight initRight list

accRight : a -> Maybe a -> Maybe a
accRight x acc =
    case acc of
        Nothing -> Just x
        Just _ -> acc

initRight : Maybe a
initRight = Nothing

flattenList : List (List a) -> List a
flattenList list =
    case list of 
        [] -> []
        [x] -> x
        x :: xs ->
            List.append x (flattenList xs)

type Tree a
    = Empty 
    | Node a (Tree a) (Tree a)

depthOfThree : Tree a -> Int
depthOfThree tree = 
    case tree of
        Empty -> 0
        Node _ left right ->
            1 + max (depthOfThree left) (depthOfThree right)

tree1 : Tree Int
tree1 = Node 1 Empty Empty

tree2 : Tree Int
tree2 = Node 1 (Node 2 Empty Empty) Empty

tree3 : Tree Int
tree3 = Node 1 (Node 2 Empty (Node 3 Empty Empty)) (Node 4 Empty Empty)

tree4 : Tree Int
tree4 = Empty

tree5 : Tree Int
tree5 = Node 1 (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty)) (Node 5 Empty Empty)

elemExists : a -> List a -> Bool
elemExists target list =
    case list of
        [] -> False
        x :: xs -> (x == target) || elemExists target xs

mapList : (a -> b) -> List a -> List b
mapList f list =
    case list of
        [] -> []
        x :: xs -> f x :: mapList f xs

longestSubList : List (List a) -> Maybe (List a)
longestSubList list =
    case list of
        [] -> Nothing
        [x] -> Just x 
        x :: xs ->
            case longestSubList xs of
                Nothing -> Just x
                Just longest ->
                    if List.length x > List.length longest then
                        Just x
                    else
                        Just longest
                    
mergeSorted : List comparable -> List comparable -> List comparable
mergeSorted list1 list2 =
    case (list1, list2) of
        ([], _) -> list2
        (_, []) -> list1
        (x :: xs, y :: ys) ->
            if x <= y then
                x :: mergeSorted xs list2
            else
                y :: mergeSorted list1 ys
            

zipLists : List a -> List b -> List (a, b)
zipLists list1 list2 =
    case (list1, list2) of
        ([], _) -> []
        (_, []) -> []
        (x :: xs, y :: ys) -> (x, y) :: zipLists xs ys

inOrder : Tree a -> List a
inOrder tree =
    case tree of
        Empty -> []
        Node value left right ->
            inOrder left ++ [value] ++ inOrder right

isBalanced : Tree a -> Bool
isBalanced tree =
    let
        heightAndBalance t =
            case t of
                Empty -> (0, True)
                Node _ left right ->
                    let
                        (leftHeight, leftBalanced) = heightAndBalance left
                        (rightHeight, rightBalanced) = heightAndBalance right
                    in
                    ( 1 + max leftHeight rightHeight
                    , leftBalanced && rightBalanced && abs (leftHeight - rightHeight) <= 1
                    )
    in
    snd (heightAndBalance tree)


minimumGain: List Float -> Maybe Float
--- Your definition of "minimumGain"
minimumGain list =
    case list of 
        [] -> Nothing
        [x] -> 
            if x > 0 then
                Just x 
            else 
                Nothing
        head :: tail ->
            let
                restMinimum = minimumGain tail 
            in 
            case restMinimum of 
                Just minGain ->
                    if head > 0 && head < minGain then 
                        Just head 
                    else 
                        Just minGain
                    
                Nothing -> 
                    if head > 0 then 
                        Just head 
                    else 
                        Nothing