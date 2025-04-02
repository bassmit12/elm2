module BinarySearchTree exposing (..)

type Tree = Nil | Node Int Tree Tree

nil : Tree
nil =
    Nil

node : Int -> Tree -> Tree -> Tree
node value left right =
    Node value left right

toString : Tree -> String
toString tree =
    case tree of
        Nil ->
            "Nil"
        Node value left right ->
            "Node " ++ String.fromInt value ++ " (" ++ toString left ++ ") (" ++ toString right ++ ")"

exampleTree : Tree
exampleTree =
    node 5 nil (node 10 nil nil)

insert : Int -> Tree -> Tree
