module Area exposing (..)

type Shape = Circle Float | Rectangle Float Float

area : Shape -> Float
area shape =
    case shape of
        Circle r ->
            3.14 * r * r
        Rectangle w h ->
            w * h
