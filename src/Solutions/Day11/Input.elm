module Solutions.Day11.Input exposing (..)

{-| 
-}



testInput : String
testInput =
    """Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1"""


expectedA : String
expectedA =
    "10605"


expectedB : String
expectedB =
    "2713310158"


input : String
input =
    """Monkey 0:
  Starting items:
  Operation: new = old * 17
  Test: divisible by 13
    If true: throw to monkey 6
    If false: throw to monkey 7

Monkey 1:
  Starting items: 0
  Operation: new = old + 8
  Test: divisible by 7
    If true: throw to monkey 0
    If false: throw to monkey 7

Monkey 2:
  Starting items: 95, 79
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 5
    If false: throw to monkey 3

Monkey 3:
  Starting items: 88, 50, 82, 65, 77
  Operation: new = old * 19
  Test: divisible by 2
    If true: throw to monkey 4
    If false: throw to monkey 1

Monkey 4:
  Starting items: 66, 90, 59, 90, 87, 63, 53, 88
  Operation: new = old + 7
  Test: divisible by 5
    If true: throw to monkey 1
    If false: throw to monkey 0

Monkey 5:
  Starting items: 92, 75, 62
  Operation: new = old * old
  Test: divisible by 3
    If true: throw to monkey 3
    If false: throw to monkey 4

Monkey 6:
  Starting items: 94, 86, 76, 67
  Operation: new = old + 1
  Test: divisible by 11
    If true: throw to monkey 5
    If false: throw to monkey 2

Monkey 7:
  Starting items: 57
  Operation: new = old + 2
  Test: divisible by 17
    If true: throw to monkey 6
    If false: throw to monkey 2
"""


