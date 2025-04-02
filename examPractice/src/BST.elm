module BST exposing (Tree, toString, insert, search, tr0, tr1, tr2, tr3, tr4, tr5, myResults)

{-| A Binary Search Tree (BST) where:
    - Nil represents an empty tree.
    - Node holds an integer and two subtrees.
-}
type Tree
    = Nil
    | Node Int Tree Tree


{-| Helper function to produce a string representation of the tree with indentation.
    The `depth` parameter controls the amount of indentation.
-}
toStringWithDepth : Int -> Tree -> String
toStringWithDepth depth tree =
    let
        indent =
            String.padLeft (depth * 3) " " ""
    in
    case tree of
        Nil ->
            indent ++ "Nil\n"

        Node value left right ->
            indent ++ "Node " ++ String.fromInt value ++ "\n"
                ++ toStringWithDepth (depth + 1) left
                ++ toStringWithDepth (depth + 1) right


{-| toString converts a tree into a human-readable string.
-}
toString : Tree -> String
toString tree =
    toStringWithDepth 0 tree


{-| insert adds a new integer into the BST.
    The function always inserts at a leaf and does nothing if the value already exists.
-}
insert : Int -> Tree -> Tree
insert x tree =
    case tree of
        Nil ->
            Node x Nil Nil

        Node value left right ->
            if x < value then
                Node value (insert x left) right
            else if x > value then
                Node value left (insert x right)
            else
                tree


{-| search checks if the given integer exists in the BST.
-}
search : Int -> Tree -> Bool
search x tree =
    case tree of
        Nil ->
            False

        Node value left right ->
            if x == value then
                True
            else if x < value then
                search x left
            else
                search x right



-- Example trees for testing

tr0 : Tree
tr0 =
    Nil

tr1 : Tree
tr1 =
    Node 4 Nil Nil

tr2 : Tree
tr2 =
    Node 8
        (Node 3
            (Node 1 Nil Nil)
            (Node 6 tr1 (Node 7 Nil Nil))
        )
        Nil

tr3 : Tree
tr3 =
    insert 10 tr2

tr4 : Tree
tr4 =
    insert 14 tr3

tr5 : Tree
tr5 =
    insert 13 tr4


{-| myResults is a list of strings to show the output of our functions.
    It prints the tree representations and the results of searches.
-}
myResults : List String
myResults =
    [ "-- output --\n"
    , toString tr1
    , toString tr3
    , toString tr5
    , String.fromBool (search 6 tr5)
    , String.fromBool (search 12 tr5)
    , "\n-- end --"
    ]
