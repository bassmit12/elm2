module DiffOne exposing (..)

diffOne : List comparable -> List comparable -> Bool
diffOne list1 list2 =
    case (list1, list2) of
        ([], []) ->
            False
        ([ _ ], []) ->
            True
        ([], [ _ ]) ->
            True
        ([], _ :: _ :: _) ->
            False
        (_ :: _ :: _, []) ->
            False
        (x :: xs, y :: ys) ->
            if x == y then
                diffOne xs ys
            else
                xs == ys
