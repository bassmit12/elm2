module SafeHead exposing (..)

safeHead : List a -> Maybe a
safeHead list =
  if List.head list == Nothing then Nothing else List.head list
