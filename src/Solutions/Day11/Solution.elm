module Solutions.Day11.Solution exposing (..)

{-| 
-}
import List.Extra


type alias TestFn =
    { divisibleBy : Int
    , trueIndex : Int
    , falseIndex : Int
    }


type alias Monkey =
    { index : Int
    , items : List Int
    , operation : Int -> Int
    , throwTo : TestFn
    , itemsInspected : Int
    }


throwItem : Int -> Int -> List Monkey -> List Monkey
throwItem item index monkeys =
    case List.Extra.getAt index monkeys of
        Just monkey ->
            List.Extra.setAt index { monkey | items = monkey.items ++ [ item ] } monkeys
        Nothing ->
            monkeys


runMonkeyPt2 : List Monkey -> Int -> (List Monkey, Int)
runMonkeyPt2 allMonkeys index =
    let
        modulus =
            List.map (\{ throwTo } -> throwTo.divisibleBy ) allMonkeys
                |> List.product
    in

    case List.Extra.getAt index allMonkeys of
        Just monkey ->
            let
                newMonkey =
                    { monkey | itemsInspected = monkey.itemsInspected + (List.length monkey.items)
                    , items = []}
            in
            ( List.Extra.mapAccuml (\monkeys item ->
                    let
                        newWorryLevel =
                            monkey.operation item
                        
                        managableWorryLevel =
                            modBy modulus newWorryLevel

                        receivingMonkeyIndex =
                            if (modBy monkey.throwTo.divisibleBy managableWorryLevel) == 0 then
                                monkey.throwTo.trueIndex
                            else
                                monkey.throwTo.falseIndex
                    in
                    (throwItem managableWorryLevel receivingMonkeyIndex monkeys, managableWorryLevel)
                )
                allMonkeys
                monkey.items
                    |> Tuple.first
                    |> List.Extra.setAt monkey.index newMonkey
            , index
            )
        Nothing ->
            (allMonkeys, 1)


runMonkey : List Monkey -> Int -> (List Monkey, Int)
runMonkey allMonkeys index =
    case List.Extra.getAt index allMonkeys of
        Just monkey ->
            let
                newMonkey =
                    { monkey | itemsInspected = monkey.itemsInspected + (List.length monkey.items)
                    , items = []}
            in
            ( List.Extra.mapAccuml (\monkeys item ->
                    let
                        newWorryLevel =
                            (monkey.operation item) // 3
                        receivingMonkeyIndex =
                            if (modBy monkey.throwTo.divisibleBy newWorryLevel) == 0 then
                                monkey.throwTo.trueIndex
                            else
                                monkey.throwTo.falseIndex
                    in
                    (throwItem newWorryLevel receivingMonkeyIndex monkeys, newWorryLevel)
                )
                allMonkeys
                monkey.items
                    |> Tuple.first
                    |> List.Extra.setAt monkey.index newMonkey
            , index
            )
        Nothing ->
            (allMonkeys, 1)


runRoundPt2 : List Monkey -> List Monkey
runRoundPt2 monkeys =
    List.Extra.mapAccuml runMonkeyPt2 monkeys (List.range 0 ((List.length monkeys) - 1))
        |> Tuple.first

runRound : List Monkey -> List Monkey
runRound monkeys =
    List.Extra.mapAccuml runMonkey monkeys (List.range 0 ((List.length monkeys) - 1))
        |> Tuple.first
    

runNRounds : Int -> List Monkey -> List Monkey
runNRounds n monkeys =
    case n of
        20 ->
            List.foldl (\_ list -> runRound list) monkeys (List.range 1 n)
        10000 ->
            List.foldl (\_ list -> runRoundPt2 list) monkeys (List.range 1 n)
        _ ->
            monkeys

calculateMonkeyBusiness : List Monkey -> Int
calculateMonkeyBusiness monkeys =
    case List.sortBy .itemsInspected monkeys |> List.reverse of
        first :: second :: _ ->
            first.itemsInspected * second.itemsInspected
        _ ->
            0    


initialiseMonkey : Int -> List Int -> (Int -> Int) -> TestFn -> Monkey
initialiseMonkey index intitialItems operation throwTo =
    { index = index
    , items = intitialItems
    , operation = operation
    , throwTo = throwTo
    , itemsInspected = 0
    }

initialiseThrowTo : Int -> Int -> Int -> TestFn
initialiseThrowTo divisor trueMonkey falseMonkey =
    { divisibleBy = divisor
    , trueIndex = trueMonkey
    , falseIndex = falseMonkey }


initialTestMonkeyList : List Monkey
initialTestMonkeyList =
   [ initialiseMonkey 0 [79, 98] ((*) 19) (initialiseThrowTo 23 2 3)
   , initialiseMonkey 1 [54, 65, 75, 74] ((+) 6) (initialiseThrowTo 19 2 0)
   , initialiseMonkey 2 [79, 60, 97] (\n -> n ^ 2) (initialiseThrowTo 13 1 3)
   , initialiseMonkey 3 [74] ((+) 3) (initialiseThrowTo 17 0 1)
   ] 


initialMonkeyList : List Monkey
initialMonkeyList =
    [ initialiseMonkey 0 [52, 60, 85, 69, 75, 75] ((*) 17) (initialiseThrowTo 13 6 7)
    , initialiseMonkey 1 [96, 82, 61, 99, 82, 84, 85] ((+) 8) (initialiseThrowTo 7 0 7)
    , initialiseMonkey 2 [95, 79] ((+) 6) (initialiseThrowTo 19 5 3)
    , initialiseMonkey 3 [88, 50, 82, 65, 77] ((*) 19) (initialiseThrowTo 2 4 1)
    , initialiseMonkey 4 [66, 90, 59, 90, 87, 63, 53, 88] ((+) 7) (initialiseThrowTo 5 1 0)
    , initialiseMonkey 5 [92, 75, 62] (\n -> n ^ 2) (initialiseThrowTo 3 3 4)
    , initialiseMonkey 6 [94, 86, 76, 67] ((+) 1) (initialiseThrowTo 11 5 2)
    , initialiseMonkey 7 [57] ((+) 2) (initialiseThrowTo 17 6 2)
    ]

solveA : List Monkey -> String
solveA monkeys =
    runNRounds 20 monkeys
        |> calculateMonkeyBusiness
        |> String.fromInt


solveB : List Monkey -> String
solveB monkeys =
    runNRounds 10000 monkeys
        |> calculateMonkeyBusiness
        |> String.fromInt


partA : String -> String
partA inputType =
    case inputType of
        "test" ->
            solveA initialTestMonkeyList

        "input" ->
            solveA initialMonkeyList

        _ ->
            "supported args are \"test\" or \"input\""


partB : String -> String
partB inputType =
    case inputType of
        "test" ->
            solveB initialTestMonkeyList

        "input" ->
            solveB initialMonkeyList

        _ ->
            "supported args are \"test\" or \"input\""


