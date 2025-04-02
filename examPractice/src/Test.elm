module Test exposing (..)

layoutA: List Int -> String
-- Definition of 'layoutA'...
layoutA list =
    case list of
        [] -> ""
        [x] -> "/" ++ String.fromInt x ++ "/"
        x :: xs ->
            "/" ++ String.fromInt x ++ layoutA xs



layoutB: List Int -> String
layoutB = List.foldl accLeft initLeft

-- step
accLeft : List Int -> b -> String
accLeft list operation = 
    -- "/" ++ String.fromInt number ++ layoutB (List.drop 1)
    case list of 
        [] -> ""
        [x] -> "/" ++ String.fromInt x ++ "/"

        x :: xs ->
            "/" ++ String.fromInt x ++ layoutB (List.drop 1 xs)


-- begin state
initLeft : String
initLeft = ""

-- Definitions of 'accLeft' and 'initLeft'...

layoutC: List Int -> String
layoutC = List.foldr accRight initRight

-- step
accRight : Int -> b -> String
accRight number operation = 
     "/" ++ String.fromInt number ++ layoutC (List.take 1 )

-- begin state
initRight : String
initRight = ""

-- Definitions of 'accRight' and 'initRight'...
