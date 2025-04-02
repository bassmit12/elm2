module CommaSeperatedString exposing (..)

commaSeperated : List String -> String
commaSeperated list =
  String.join "," list
