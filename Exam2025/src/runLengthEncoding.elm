module RunLengthEncoding exposing (runLengthEncoding)

runLengthEncoding : Char -> List Char -> (Int, List Char)
runLengthEncoding c list =
    case list of
        [] ->
            (0, [])
        [x] ->
            if x == c then
                (1, [])
            else
                (0, [x])
        x :: xs ->
            if x == c then
                let
                    (n, rest) =
                        runLengthEncoding c xs
                in
                (n + 1, rest)
            else
                (0, x :: xs)
