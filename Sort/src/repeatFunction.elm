module Main exposing (..)
import Debug exposing (toString)


repeatUntil : (a -> Bool) -> (a -> a) -> a -> a
repeatUntil predicate func value =
    if predicate value then
        value
    else
        repeatUntil predicate func (func value)

above100 : Int -> Bool
above100 x =
    x > 100

modulus : Int -> Int -> Int
modulus dividend divisor =
    dividend - (dividend // divisor) * divisor


aboveValue : Int -> Int -> Bool
aboveValue limit x =
    x > limit

log3Base : Int -> Int
log3Base n =
    if n < 100 then
        0
    else
        1 + log3Base (n // 3)

collatz : Int -> List Int -> List Int
collatz n sequence =
    if n == 1 then
        List.reverse (n :: sequence)
    else if modulus n 2 == 0 then
        collatz (n // 2) (n :: sequence)
    else
        collatz (3 * n + 1) (n :: sequence)


test1 : Int
test1 =
    repeatUntil above100 ((+) 1) 42

test2 : Int
test2 =
    repeatUntil (aboveValue 150) ((+) 1) 42

test3 : Int
test3 =
    log3Base 100

test4 : List Int
test4 =
    repeatUntil (\x -> List.head x == Just 1) (collatz >> List.tail) [19]

main =
    Debug.toString [ test1, test2, test3, test4 ]
