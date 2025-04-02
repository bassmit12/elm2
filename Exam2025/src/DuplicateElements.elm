module DuplicateElements exposing (..)

duplicateElements : List a -> List a
duplicateElements list =
    case list of
        [] ->
            []

        x :: xs ->
            x :: x :: duplicateElements xs
