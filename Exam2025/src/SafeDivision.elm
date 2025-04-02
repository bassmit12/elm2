module SafeDivision exposing (..)

safeDivide : Float -> Float -> Maybe Float
safeDivide float1 float2 =
  if float1 / float2 == 0 then Nothing else Just (float1 / float2)
