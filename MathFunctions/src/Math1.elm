module Main exposing (..)

type Function
    = Const Int
    | X
    | Poly Int Int
    | Plus Function Function
    | Minus Function Function
    | Mult Function Function
    | Div Function Function
    | Pow Function Int

print : Function -> String
print func =
    case func of
        Const n ->
            String.fromInt n

        X ->
            "x"

        Poly a b ->
            "("
                ++ "x ^ "
                ++ String.fromInt b
                ++ ")"

        Plus left right ->
            "("
                ++ print left
                ++ " + "
                ++ print right
                ++ ")"

        Minus left right ->
            "("
                ++ print left
                ++ " - "
                ++ print right
                ++ ")"

        Mult left right ->
            "("
                ++ print left
                ++ " * "
                ++ print right
                ++ ")"

        Div left right ->
            "("
                ++ print left
                ++ " / "
                ++ print right
                ++ ")"

        Pow base exponent ->
            "("
                ++ print base
                ++ " ^ "
                ++ String.fromInt exponent
                ++ ")"

-- Example usage
f : Function
f =
    Plus (Mult (Plus (Const 3) X) (Minus X (Poly 1 5))) (Const 2)
