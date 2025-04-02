module Caesar exposing (..)

encode : Int -> Char -> Char
encode shift character =
    if Char.isUpper character then
        let
            base = Char.toCode 'A'
            offset = Char.toCode character - base
            newOffset = modBy 26 (offset + shift)
        in
        Char.fromCode (base + newOffset)
    else if Char.isLower character then
        let
            base = Char.toCode 'a'
            offset = Char.toCode character - base
            newOffset = modBy 26 (offset + shift)
        in
        Char.fromCode (base + newOffset)
    else
        character


decode : Int -> Char -> Char
decode shift character =
    encode (-shift) character
