module Main exposing (..)

--Make sure values loop!
encode : Int -> Char -> Char
encode shiftAmount character =
    let
        initialCharCode = Char.toCode character
        shiftedCharCode = 
            if Char.isUpper character && (initialCharCode + shiftAmount) > 90 then
                initialCharCode + shiftAmount - 26
            else if Char.isLower character && (initialCharCode + shiftAmount) > 122 then
                initialCharCode + shiftAmount - 26
            else
                initialCharCode + shiftAmount
        encodedChar = Char.fromCode shiftedCharCode
    in
    encodedChar


decode : Int -> Char -> Char
decode shiftAmount character =
    let
        initialCharCode = Char.toCode character
        shiftedCharCode =
            if Char.isUpper character && (initialCharCode - shiftAmount) < 65 then
                initialCharCode - shiftAmount + 26
            else if Char.isLower character && (initialCharCode - shiftAmount) < 97 then
                initialCharCode - shiftAmount + 26
            else
                initialCharCode - shiftAmount
        decodedChar = Char.fromCode shiftedCharCode
    in
    decodedChar



-- Tests
testEncodeA : Bool
testEncodeA = (encode 1 'A' == 'B')

testEncodeB : Bool
testEncodeB = (encode 2 'z' == 'b')

testDecodeA : Bool
testDecodeA = (decode 2 'D' == 'B')

testDecodeB : Bool
testDecodeB = (decode 1 'a' == 'z')

allTests : List Bool
allTests = [testEncodeA, testEncodeB, testDecodeA, testDecodeB]
