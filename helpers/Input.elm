module Input exposing (..)

{-| 
-}



testInput : String
testInput =
    "replace me with the test input"


expectedA : String
expectedA =
    ""


expectedB : String
expectedB =
    ""


input : String
input =
    """    [P]                 [Q]     [T]
[F] [N]             [P] [L]     [M]
[H] [T] [H]         [M] [H]     [Z]
[M] [C] [P]     [Q] [R] [C]     [J]
[T] [J] [M] [F] [L] [G] [R]     [Q]
[V] [G] [D] [V] [G] [D] [N] [W] [L]
[L] [Q] [S] [B] [H] [B] [M] [L] [D]
[D] [H] [R] [L] [N] [W] [G] [C] [R]
 1   2   3   4   5   6   7   8   9 

move 1 from 7 to 6
move 1 from 8 to 5
move 3 from 7 to 4
move 5 from 9 to 6
move 3 from 7 to 9
move 2 from 5 to 7
move 10 from 6 to 8
move 2 from 2 to 3
move 2 from 9 to 1
move 6 from 8 to 2
move 5 from 3 to 8
move 4 from 5 to 9
move 3 from 4 to 5
move 2 from 1 to 8
move 3 from 1 to 7
move 1 from 7 to 1
move 4 from 7 to 8
move 1 from 5 to 6
move 1 from 9 to 3
move 8 from 2 to 4
move 1 from 5 to 8
move 1 from 5 to 3
move 2 from 1 to 8
move 4 from 3 to 4
move 1 from 3 to 4
move 1 from 1 to 7
move 1 from 7 to 8
move 1 from 7 to 4
move 5 from 9 to 1
move 2 from 6 to 7
move 3 from 2 to 1
move 12 from 8 to 7
move 8 from 7 to 3
move 1 from 2 to 8
move 6 from 7 to 1
move 1 from 6 to 3
move 8 from 4 to 3
move 5 from 3 to 6
move 6 from 1 to 8
move 2 from 1 to 2
move 2 from 3 to 1
move 4 from 4 to 5
move 1 from 5 to 7
move 1 from 6 to 9
move 1 from 4 to 9
move 8 from 1 to 4
move 10 from 3 to 5
move 2 from 4 to 5
move 2 from 2 to 6
move 2 from 1 to 6
move 11 from 4 to 7
move 9 from 6 to 5
move 16 from 8 to 3
move 15 from 5 to 6
move 10 from 3 to 6
move 24 from 6 to 5
move 5 from 7 to 5
move 1 from 6 to 3
move 1 from 7 to 2
move 2 from 7 to 6
move 3 from 3 to 6
move 8 from 5 to 1
move 3 from 9 to 8
move 3 from 8 to 4
move 1 from 7 to 1
move 1 from 2 to 9
move 1 from 9 to 2
move 2 from 3 to 1
move 2 from 4 to 2
move 5 from 6 to 8
move 3 from 7 to 1
move 1 from 4 to 2
move 26 from 5 to 9
move 1 from 3 to 6
move 7 from 1 to 9
move 1 from 3 to 5
move 1 from 6 to 5
move 1 from 5 to 4
move 5 from 5 to 6
move 1 from 4 to 9
move 3 from 9 to 3
move 4 from 8 to 5
move 2 from 5 to 2
move 1 from 1 to 6
move 1 from 8 to 9
move 2 from 2 to 4
move 2 from 3 to 7
move 1 from 7 to 6
move 7 from 6 to 7
move 1 from 4 to 3
move 2 from 2 to 4
move 28 from 9 to 3
move 26 from 3 to 7
move 2 from 4 to 3
move 2 from 9 to 1
move 4 from 3 to 6
move 1 from 4 to 5
move 1 from 3 to 4
move 3 from 1 to 9
move 1 from 4 to 7
move 1 from 5 to 7
move 1 from 6 to 9
move 23 from 7 to 1
move 4 from 9 to 5
move 3 from 9 to 4
move 2 from 6 to 3
move 1 from 6 to 7
move 3 from 3 to 9
move 11 from 7 to 2
move 4 from 2 to 3
move 23 from 1 to 2
move 15 from 2 to 4
move 2 from 7 to 9
move 13 from 2 to 8
move 1 from 7 to 5
move 1 from 2 to 8
move 7 from 4 to 8
move 6 from 4 to 3
move 1 from 2 to 4
move 1 from 2 to 9
move 20 from 8 to 5
move 1 from 8 to 4
move 3 from 4 to 7
move 3 from 3 to 9
move 1 from 2 to 8
move 20 from 5 to 3
move 6 from 5 to 3
move 26 from 3 to 9
move 2 from 7 to 5
move 1 from 5 to 4
move 1 from 7 to 8
move 2 from 8 to 5
move 12 from 9 to 4
move 2 from 3 to 2
move 4 from 1 to 9
move 2 from 3 to 1
move 4 from 5 to 6
move 5 from 9 to 4
move 2 from 6 to 3
move 2 from 6 to 8
move 2 from 8 to 3
move 1 from 2 to 7
move 21 from 4 to 2
move 1 from 4 to 5
move 13 from 2 to 4
move 4 from 3 to 9
move 25 from 9 to 7
move 7 from 2 to 4
move 18 from 7 to 8
move 2 from 1 to 5
move 1 from 3 to 9
move 2 from 9 to 3
move 1 from 1 to 6
move 8 from 7 to 6
move 4 from 3 to 2
move 1 from 4 to 7
move 6 from 2 to 5
move 1 from 7 to 3
move 5 from 6 to 8
move 4 from 4 to 1
move 9 from 5 to 1
move 12 from 4 to 3
move 1 from 6 to 5
move 1 from 5 to 2
move 13 from 3 to 8
move 14 from 8 to 6
move 2 from 1 to 6
move 1 from 2 to 5
move 11 from 1 to 3
move 1 from 5 to 3
move 6 from 6 to 8
move 23 from 8 to 5
move 1 from 8 to 1
move 18 from 5 to 8
move 5 from 6 to 8
move 10 from 3 to 8
move 1 from 1 to 5
move 2 from 4 to 8
move 1 from 4 to 7
move 5 from 5 to 3
move 1 from 6 to 1
move 6 from 3 to 9
move 35 from 8 to 4
move 1 from 7 to 6
move 2 from 9 to 8
move 1 from 1 to 6
move 17 from 4 to 7
move 1 from 5 to 1
move 4 from 9 to 6
move 12 from 6 to 4
move 29 from 4 to 2
move 17 from 7 to 8
move 27 from 2 to 7
move 2 from 2 to 1
move 1 from 3 to 1
move 25 from 7 to 4
move 25 from 4 to 6
move 1 from 4 to 2
move 4 from 1 to 6
move 1 from 2 to 6
move 25 from 6 to 1
move 5 from 6 to 8
move 15 from 1 to 6
move 2 from 7 to 8
move 15 from 6 to 2
move 14 from 2 to 8
move 1 from 2 to 3
move 4 from 1 to 4
move 4 from 4 to 2
move 6 from 1 to 8
move 3 from 2 to 5
move 3 from 5 to 7
move 1 from 2 to 3
move 1 from 6 to 8
move 8 from 8 to 5
move 2 from 7 to 4
move 1 from 7 to 9
move 3 from 5 to 8
move 2 from 4 to 6
move 3 from 5 to 8
move 2 from 3 to 4
move 2 from 6 to 5
move 1 from 9 to 8
move 48 from 8 to 5
move 1 from 8 to 9
move 41 from 5 to 4
move 4 from 5 to 2
move 3 from 2 to 7
move 1 from 2 to 7
move 1 from 8 to 1
move 1 from 9 to 4
move 1 from 1 to 3
move 7 from 4 to 7
move 11 from 7 to 4
move 4 from 4 to 1
move 37 from 4 to 9
move 4 from 4 to 3
move 32 from 9 to 3
move 5 from 9 to 1
move 12 from 3 to 2
move 3 from 4 to 1
move 3 from 1 to 6
move 3 from 1 to 6
move 2 from 1 to 5
move 9 from 2 to 7
move 3 from 7 to 3
move 6 from 6 to 5
move 4 from 3 to 6
move 3 from 6 to 9
move 13 from 3 to 8
move 3 from 1 to 9
move 2 from 3 to 2
move 2 from 7 to 8
move 1 from 6 to 8
move 4 from 2 to 8
move 2 from 8 to 3
move 1 from 2 to 1
move 4 from 7 to 3
move 6 from 3 to 5
move 3 from 9 to 8
move 13 from 8 to 6
move 1 from 9 to 2
move 2 from 3 to 8
move 1 from 1 to 9
move 1 from 1 to 3
move 10 from 6 to 3
move 1 from 2 to 5
move 22 from 5 to 7
move 1 from 9 to 3
move 1 from 8 to 7
move 2 from 7 to 8
move 6 from 8 to 4
move 2 from 9 to 2
move 21 from 7 to 6
move 4 from 8 to 5
move 1 from 8 to 4
move 1 from 5 to 7
move 12 from 3 to 6
move 1 from 2 to 6
move 1 from 7 to 9
move 1 from 2 to 6
move 6 from 3 to 5
move 6 from 4 to 2
move 1 from 3 to 6
move 1 from 9 to 7
move 6 from 2 to 7
move 22 from 6 to 4
move 3 from 6 to 5
move 7 from 5 to 7
move 3 from 7 to 8
move 2 from 5 to 3
move 2 from 3 to 7
move 13 from 6 to 8
move 3 from 7 to 1
move 3 from 5 to 9
move 16 from 4 to 5
move 1 from 5 to 8
move 2 from 1 to 6
move 1 from 1 to 7
move 6 from 4 to 2
move 4 from 8 to 7
move 13 from 5 to 7
move 1 from 6 to 3
move 2 from 5 to 6
move 10 from 7 to 6
move 1 from 3 to 9
move 1 from 4 to 3
move 1 from 3 to 5
move 12 from 7 to 3
move 2 from 2 to 1
move 1 from 5 to 9
move 2 from 9 to 6
move 4 from 2 to 7
move 7 from 7 to 9
move 1 from 7 to 8
move 1 from 1 to 9
move 11 from 9 to 7
move 4 from 8 to 3
move 5 from 3 to 5
move 2 from 8 to 4
move 3 from 5 to 2
move 2 from 2 to 8
move 1 from 5 to 2
move 5 from 8 to 2
move 7 from 7 to 2
move 4 from 8 to 9
move 2 from 7 to 6
move 4 from 9 to 7
move 6 from 2 to 4
move 1 from 5 to 6
move 5 from 3 to 5
move 1 from 8 to 1
move 10 from 6 to 3
move 8 from 2 to 8
move 1 from 8 to 1
move 5 from 3 to 2
move 2 from 8 to 7
move 6 from 7 to 4
move 12 from 4 to 1
move 4 from 1 to 2
move 1 from 2 to 1
move 8 from 2 to 9
move 2 from 4 to 8
move 5 from 9 to 7
move 8 from 3 to 8
move 2 from 3 to 1
move 6 from 8 to 2
move 7 from 7 to 2
move 1 from 3 to 5
move 2 from 7 to 2
move 1 from 9 to 1
move 1 from 9 to 7
move 1 from 9 to 4
move 1 from 6 to 7
move 1 from 2 to 3
move 1 from 3 to 8
move 1 from 4 to 9
move 5 from 6 to 1
move 7 from 8 to 2
move 1 from 7 to 4
move 9 from 2 to 8
move 7 from 2 to 7
move 1 from 4 to 2
move 8 from 7 to 5
move 4 from 8 to 7
move 8 from 8 to 6
move 9 from 1 to 4
move 1 from 9 to 1
move 4 from 7 to 6
move 7 from 1 to 7
move 6 from 7 to 3
move 4 from 1 to 8
move 13 from 6 to 3
move 6 from 2 to 3
move 1 from 3 to 4
move 2 from 3 to 7
move 1 from 6 to 9
move 11 from 5 to 1
move 1 from 6 to 3
move 8 from 4 to 1
move 2 from 5 to 2
move 1 from 9 to 5
move 2 from 8 to 7
move 7 from 1 to 5
move 2 from 7 to 3
move 8 from 5 to 4
move 1 from 8 to 2
move 1 from 5 to 7
move 3 from 7 to 2
move 4 from 4 to 7
move 4 from 3 to 4
move 20 from 3 to 2
move 1 from 8 to 3
move 1 from 3 to 8
move 4 from 7 to 2
move 1 from 8 to 6
move 1 from 7 to 5
move 1 from 3 to 1
move 1 from 4 to 2
move 5 from 1 to 4
move 14 from 4 to 1
move 1 from 6 to 5
move 1 from 2 to 3
move 1 from 5 to 1
move 11 from 2 to 9
move 18 from 1 to 2
move 4 from 1 to 3
move 12 from 2 to 5
move 5 from 2 to 4
move 7 from 5 to 1
move 1 from 2 to 9
move 9 from 1 to 9
move 1 from 3 to 6
move 2 from 3 to 9
move 1 from 6 to 1
move 1 from 4 to 8
move 1 from 3 to 4
move 1 from 3 to 8
move 16 from 9 to 5
move 2 from 2 to 7
move 14 from 5 to 8
move 16 from 8 to 5
move 1 from 7 to 9
move 1 from 7 to 6
move 4 from 9 to 5
move 11 from 5 to 6
move 12 from 2 to 4
move 16 from 5 to 7
move 4 from 7 to 2
move 1 from 5 to 6
move 3 from 9 to 1
move 4 from 7 to 9
move 3 from 6 to 4
move 9 from 2 to 9
move 3 from 1 to 8
move 2 from 8 to 1
move 1 from 8 to 2
move 5 from 6 to 1
move 7 from 7 to 1
move 1 from 7 to 6
move 8 from 4 to 5
move 1 from 2 to 6
move 12 from 9 to 2
move 3 from 2 to 9
move 8 from 5 to 8
move 12 from 4 to 5
move 1 from 2 to 9
move 1 from 5 to 6
move 2 from 1 to 7
move 4 from 5 to 2
move 6 from 5 to 1
move 2 from 7 to 6
move 1 from 5 to 1
move 1 from 8 to 5
move 7 from 6 to 9
move 2 from 9 to 4
move 16 from 1 to 8
move 1 from 5 to 8
move 7 from 2 to 8
move 3 from 6 to 2
move 1 from 4 to 8
move 28 from 8 to 3
move 1 from 4 to 2
move 4 from 1 to 2
move 11 from 2 to 7
move 9 from 7 to 8
move 7 from 9 to 5
move 4 from 8 to 1
move 2 from 9 to 1
move 2 from 1 to 5
move 1 from 7 to 9
move 1 from 1 to 9
move 6 from 5 to 3
move 3 from 5 to 1
move 2 from 2 to 8
move 7 from 8 to 3
move 7 from 3 to 7
move 4 from 1 to 9
move 1 from 8 to 9
move 2 from 8 to 1
move 1 from 8 to 1
move 6 from 7 to 6
move 6 from 6 to 5
move 17 from 3 to 6
move 2 from 9 to 2
move 2 from 1 to 4
move 12 from 3 to 8
move 6 from 6 to 5
move 2 from 2 to 1
move 4 from 9 to 7
move 2 from 7 to 3
move 1 from 1 to 5
move 10 from 8 to 6
move 2 from 3 to 9
move 9 from 5 to 2
move 7 from 2 to 8
move 1 from 4 to 8
move 1 from 4 to 6
move 7 from 8 to 7
move 3 from 9 to 7
move 4 from 3 to 4
"""


