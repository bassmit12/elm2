module Caesar2 exposing (..)

-- Define a function to check if a character is a letter (A-Z or a-z)
isCharacter : Char -> Bool
isCharacter char =
    let
        encodedChar = Char.toCode char
    in
    if encodedChar >= 65 && encodedChar <= 90 then
        True
    else if encodedChar >= 97 && encodedChar <= 122 then
        True
    else
        False

-- Define a function to normalize a string by removing non-letter characters
normalize : String -> String
normalize string =
   String.filter isCharacter string

-- Define a function to encode a character using a Caesar cipher with a given shift number
encodeChar : Int -> Char -> Char
encodeChar shiftNumber character =
    if Char.isAlpha character then
        let 
            baseCode = if Char.isUpper character then 65 else 97 -- ASCII code for 'A' or 'a'
            characterCode = Char.toCode character - baseCode -- Convert character to ASCII code relative to 'A' or 'a'
            shiftedCharacterCode = (modBy 26 (characterCode + shiftNumber)) + baseCode -- Apply Caesar shift
        in Char.fromCode shiftedCharacterCode -- Convert shifted ASCII code back to character
    else character -- Return non-letter characters unchanged

-- Define a function to decode a character using a Caesar cipher with a given shift number
decodeChar : Int -> Char -> Char
decodeChar shiftNumber character =
    encodeChar (negate shiftNumber) character -- Decoding is the same as encoding with the opposite shift

-- Define a function to apply an encoding or decoding operation to a list of characters
applyOperation : (Int -> Char -> Char) -> Int -> List Char -> List Char
applyOperation operation shiftNumber charList =
    case charList of
        [] ->
            []

        firstChar :: restOfChars ->
            operation shiftNumber firstChar :: applyOperation operation shiftNumber restOfChars

-- Define a function to encrypt a string using a Caesar cipher with a given shift number
encrypt : Int -> String -> String
encrypt shiftNumber string =
    let
        listString = String.toList string -- Convert string to a list of characters
        encodedListString = applyOperation encodeChar shiftNumber listString -- Apply encoding operation to each character
    in
    String.fromList encodedListString -- Convert encoded list back to a string

-- Define a function to decrypt a string using a Caesar cipher with a given shift number
decrypt : Int -> String -> String
decrypt shiftNumber string =
    let
        listString = String.toList string -- Convert string to a list of characters
        decodedListString = applyOperation decodeChar shiftNumber listString -- Apply decoding operation to each character
    in
    String.fromList decodedListString -- Convert decoded list back to a string

-- Define unit tests for the functions
runTests : List Bool
runTests =       
    let
        tests =
            [
                ("Test normalize with empty string", normalize "" == ""),
                ("Test normalize with only alphabetic characters", normalize "Hello" == "Hello"),
                ("Test normalize with non-alphabetic characters", normalize " H#, =, !, @e 1l,l.!o2" == "Hello"),

                ("Test encrypt with shift 3 and empty string", encrypt 3 "" == ""),
                ("Test encrypt with shift 3 and unencryptedString 'Hello'", encrypt 3 "Hello" == "Khoor"),
                ("Test encrypt with shift 3 and unencryptedString 'abc'", encrypt 3 "abc" == "def"),

                ("Test decrypt with shift 3 and empty string", decrypt 3 "" == ""),
                ("Test decrypt with shift 3 and encryptedString 'Khoor'", decrypt 3 "Khoor" == "Hello"),
                ("Test decrypt with shift 3 and encryptedString 'def'", decrypt 3 "def" == "abc")
            ]
    in
    List.map (\(desc, result) -> Debug.log desc result) (List.reverse tests)
