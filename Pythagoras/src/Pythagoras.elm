module Pythagoras exposing (..)

sqr: Int -> Int
sqr number = 
  let 
      square = number * number
  in
  
  square


isTriple: Int -> Int -> Int -> Bool
isTriple a b c =
  if sqr a + sqr b == sqr c && (a > 0 && b > 0 && c > 0) then
    True
  else
    False

leg1 : Int -> Int -> Int
leg1 x y =
    sqr x - sqr y

leg2 : Int -> Int -> Int
leg2 x y =
    2 * x * y

hyp : Int -> Int -> Int
hyp x y =
    sqr x + sqr y

pythTriple : (Int, Int) -> (Int, Int, Int)
pythTriple (x, y) =
    (leg1 x y, leg2 x y, hyp x y)

isTripleTuple : (Int, Int, Int) -> Bool
isTripleTuple (a, b, c) =
    isTriple a b c


testLeg1 : Bool
testLeg1 = (leg1 5 4 == 9)

testLeg2 : Bool
testLeg2 = (leg2 5 4 == 40)

testHyp : Bool
testHyp = (hyp 5 4 == 41)

testIsTriple : Bool
testIsTriple = (isTriple 9 40 41 == True)

testPythTriple : Bool
testPythTriple = (pythTriple (5,4) == (9, 40, 41))

testIsTripleTuple : Bool
testIsTripleTuple = (isTripleTuple (9, 40, 41) == True)

allTests : List Bool
allTests = [testLeg1, testLeg2, testHyp, testIsTriple, testPythTriple, testIsTripleTuple]
