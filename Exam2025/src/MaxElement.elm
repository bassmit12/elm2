module MaxElement exposing (..)

maxElement : List Int -> Maybe Int
maxElement list = 
    case list of
        [] -> 
            Nothing
        [x] -> 
            Just x
        x :: xs -> 
            case maxElement xs of
                Nothing -> 
                    Just x
                Just y -> 
                    Just (max x y)