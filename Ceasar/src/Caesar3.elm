module Caesar3 exposing (..)


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


-- Function to check if the list contains at least one True value
listContainsTrue : List Bool -> Bool
listContainsTrue boolList =
    List.any (\x -> x == True) boolList

findCandidates : String -> List String -> Bool
findCandidates string listOfSearchStrings =
  let
    boolList = List.map (\searchString -> String.contains searchString string) listOfSearchStrings
  in 
    listContainsTrue boolList

findCandidatesRec : String -> List String -> Bool
findCandidatesRec string listOfSearchStrings =
    case listOfSearchStrings of
        [] ->
            False

        firstSearchString :: restOfSearchStrings ->
            String.contains firstSearchString string || findCandidatesRec string restOfSearchStrings

candidates : List String -> String -> List (Int, String)
candidates listOfSearchStrings string =
    let
        possibleKeys = List.range 0 25 -- List of possible shift values (0 to 25)
        allLists = List.map (\key -> (key, encrypt key string)) possibleKeys
        passingLists = List.filter (\(key, encrypted) -> findCandidatesRec encrypted listOfSearchStrings) allLists
    in
        passingLists
