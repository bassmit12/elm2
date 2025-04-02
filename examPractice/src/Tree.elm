module Tree exposing (..)

-- Type definition
type Expr a b
    = Leaf a
    | Node b (Expr a b) (Expr a b)

-- Function to calculate the height of the expression tree
height : Expr a b -> Int
height expr =
    case expr of
        Leaf _ -> 1
        Node _ left right ->
            1 + max (height left) (height right)

-- Function to evaluate a Boolean expression tree
boolEval : Expr Bool String -> Maybe Bool
boolEval expr =
    case expr of
        Leaf value ->
            Just value
        
        Node op left right ->
            case (boolEval left, boolEval right) of
                (Just l, Just r) ->
                    case op of
                        "AND" -> Just (l && r)
                        "OR" -> Just (l || r)
                        _ -> Nothing
                _ ->
                    Nothing