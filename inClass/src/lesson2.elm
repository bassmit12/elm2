module lesson2 exposing (..)

recursion: Int -> Int
recursion int =
  if int <= 1 then
    1
  else
    int * int + recursion (int - 1)


