module Pythagoras2 exposing (..)

-- Calculate the first leg of a right triangle given two sides
leg1 : Int -> Int -> Int
leg1 x y =
    x ^ 2 - y ^ 2

-- Calculate the second leg of a right triangle given two sides
leg2 : Int -> Int -> Int
leg2 x y =
    2 * x * y

-- Calculate the hypotenuse of a right triangle given two sides
hyp : Int -> Int -> Int
hyp x y =
    x ^ 2 + y ^ 2

-- Generate a Pythagorean triple given two sides of a right triangle
pythTriple : (Int, Int) -> (Int, Int, Int)
pythTriple (x, y) =
    (leg1 x y, leg2 x y, hyp x y)

-- Check if a triple is a valid Pythagorean triple
isTriple : (Int, Int, Int) -> Bool
isTriple (a, b, c) =
    a ^ 2 + b ^ 2 == c ^ 2 && (a > 0 && b > 0 && c > 0)

-- Map a list of pairs of integers to a list of Pythagorean triples
pythTriplesMap : List (Int, Int) -> List (Int, Int, Int)
pythTriplesMap pairs =
    List.map pythTriple pairs

-- Generate Pythagorean triples recursively from a list of pairs
pythTriplesRec : List (Int, Int) -> List (Int, Int, Int)
pythTriplesRec pairs =
    case pairs of
        [] ->
            []

        firstPair :: restOfPairs ->
            pythTriple firstPair :: pythTriplesRec restOfPairs

-- Filter a list of triples to include only Pythagorean triples
arePythTriplesFilter : List (Int, Int, Int) -> List (Int, Int, Int)
arePythTriplesFilter pairs =
    List.filter isTriple pairs

-- Generate Pythagorean triples recursively from a list of pairs
arePythTriplesRec : List (Int, Int, Int) -> List (Int, Int, Int)
arePythTriplesRec pairs =
    case pairs of
        [] ->
            []

        firstPair :: restOfPairs ->
            if isTriple firstPair then
                firstPair :: arePythTriplesRec restOfPairs
            else
                arePythTriplesRec restOfPairs

-- Unit tests
runTests : List Bool
runTests =
    let
        pairs = [(5, 4), (2, 1), (35, 7)] -- Sample input pairs
        triples = pythTriplesMap pairs -- Generate triples using mapping
        triplesRec = pythTriplesRec pairs -- Generate triples using recursion
        arePyth = arePythTriplesFilter triples -- Filter valid Pythagorean triples
        arePythRec = arePythTriplesRec triplesRec -- Filter valid Pythagorean triples recursively
    in
    [ -- Check if generated triples are Pythagorean triples
        List.all isTriple triples,
        List.all isTriple triplesRec,
        -- Check if filtered triples are indeed Pythagorean triples
        List.all isTriple arePyth,
        List.all isTriple arePythRec
    ]
