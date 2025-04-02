module CreditCard exposing (..)

-- The numValid is of a different type than the example

reverseList: List Int -> List Int
reverseList list = 
  case list of 
    [] -> []

    firstNumber :: restOfNumbers ->
      reverseList restOfNumbers ++ [firstNumber]

doubleEveryOtherNumber: List Int -> List Int 
doubleEveryOtherNumber list =
  case list of
    [] -> []
    [firstElement] -> [firstElement]

    firstElement :: numberToDouble :: restOfNumbers ->
      firstElement :: numberToDouble * 2 :: doubleEveryOtherNumber restOfNumbers 

creditCard: List Int -> Int
creditCard list = 
  let 
      reversedList = reverseList list 
      doubleEveryOtherNumberList = doubleEveryOtherNumber reversedList
      slicedList = List.map (\x -> sliceNumber x) doubleEveryOtherNumberList
      concatList = List.concat slicedList
      summedList = sumList concatList
  in
  
  modBy 10 summedList  
 
sumList: List Int -> Int
sumList list =
  case list of 
    [] -> 0

    firstNumber :: restOfNumbers ->
      firstNumber + sumList restOfNumbers

  -- List.foldl (+) 0 list would work aswell


sliceNumber : Int -> List Int
sliceNumber number =
    let
        modNumber = modBy 10 number
        remainder = number // 10
    in
    if remainder < 10 then
        [modNumber, remainder]
    else
        [modNumber] ++ sliceNumber remainder


numValid : List (List Int) -> Int
numValid creditCards =
    let
        filteredList = List.filter (\x -> creditCard x == 0) creditCards
    in
    List.length filteredList


testReverseList : Bool
testReverseList =
    reverseList [1, 2, 3] == [3, 2, 1]

testDoubleEveryOtherNumber : Bool
testDoubleEveryOtherNumber =
    doubleEveryOtherNumber [1, 2, 3, 4] == [1, 4, 3, 8]

testSliceNumber : Bool
testSliceNumber =
    sliceNumber 123 == [3, 2, 1]

testCreditCardValid : Bool
testCreditCardValid =
    creditCard [4, 0, 1, 2, 8, 8, 8, 8, 8, 8, 8, 8, 1, 8, 8, 1] == 0

testCreditCardInvalid : Bool
testCreditCardInvalid =
    creditCard [4, 0, 1, 2, 8, 8, 8, 8, 8, 8, 8, 8, 1, 8, 9, 1] /= 0

testNumValid : Bool
testNumValid =
    numValid [[4, 0, 1, 2, 8, 8, 8, 8, 8, 8, 8, 8, 1, 8, 8, 1], [4, 0, 1, 2, 8, 8, 8, 8, 8, 8, 8, 8, 1, 8, 9, 1]] == 1

runAllTests : List Bool
runAllTests =
    [ testReverseList
    , testDoubleEveryOtherNumber
    , testSliceNumber
    , testCreditCardValid
    , testCreditCardInvalid
    , testNumValid
    ]
