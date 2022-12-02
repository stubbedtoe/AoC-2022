module Solutions.Day2.Solution exposing (..)

{-| 
-}


import Solutions.Day2.Input as Input

{- 
opponent:
A = Rock
B = Paper
C = Scissors

you:
X = Rock
Y = Paper
Z = Scissors

scoring:

Rock = 1
Paper = 2
Scissors = 3

Loss = 0
Draw = 3
Win = 6
-}

type Hand = Rock | Paper | Scissors

type RequiredResult = Win | Loss | Draw

scoreRound : (Hand, Hand) -> Int
scoreRound (opponent, you) =
    case opponent of
        Rock ->
            case you of
                Rock ->
                    3 + 1
                Paper ->
                    6 + 2
                Scissors ->
                    0 + 3
        Paper ->
            case you of
                Rock ->
                    0 + 1
                Paper ->
                    3 + 2
                Scissors ->
                    6 + 3
        Scissors ->
            case you of
                Rock ->
                    6 + 1
                Paper ->
                    0 + 2
                Scissors ->
                    3 + 3 


requiredResultToRound : (Hand, RequiredResult) -> (Hand, Hand)
requiredResultToRound (opponent, result) =
    let 
        you =
            case opponent of
                Rock ->
                    case result of
                        Win ->
                            Paper
                        Loss ->
                            Scissors
                        Draw ->
                            opponent
                Paper ->
                    case result of
                        Win ->
                            Scissors
                        Loss ->
                            Rock
                        Draw ->
                            opponent
                Scissors ->
                    case result of
                        Win ->
                            Rock
                        Loss ->
                            Paper
                        Draw ->
                            opponent
    in
    (opponent, you)



lineToRound : String -> Maybe (Hand, Hand)
lineToRound line =
    let 
        split =
            String.split " " line
        opponent =
            case split of
                [ "A", _ ] ->
                    Just Rock
                [ "B", _ ] ->
                    Just Paper
                [ "C", _ ] ->
                    Just Scissors
                _ ->
                    Nothing
        you =
            case split of
                [ _, "X" ] ->
                    Just Rock
                [ _, "Y" ] ->
                    Just Paper
                [ _, "Z" ] ->
                    Just Scissors
                _ ->
                    Nothing
    in
    case (opponent, you) of
        (Just hand1, Just hand2) ->
            Just (hand1, hand2)
        _ ->
            Nothing

lineToRoundPt2 : String -> Maybe (Hand, RequiredResult)
lineToRoundPt2 line =
    let 
        split =
            String.split " " line
        opponent =
            case split of
                [ "A", _ ] ->
                    Just Rock
                [ "B", _ ] ->
                    Just Paper
                [ "C", _ ] ->
                    Just Scissors
                _ ->
                    Nothing
        required =
            case split of
                [ _, "X" ] ->
                    Just Loss
                [ _, "Y" ] ->
                    Just Draw
                [ _, "Z" ] ->
                    Just Win
                _ ->
                    Nothing
    in
    case (opponent, required) of
        (Just hand, Just result) ->
            Just (hand, result)
        _ ->
            Nothing


solveA : String -> String
solveA input =
    String.lines input
        |> List.filterMap lineToRound
        |> List.map scoreRound
        |> List.sum
        |> String.fromInt


solveB : String -> String
solveB input =
    String.lines input
        |> List.filterMap lineToRoundPt2
        |> List.map requiredResultToRound
        |> List.map scoreRound
        |> List.sum
        |> String.fromInt


partA : String -> String
partA inputType =
    case inputType of
        "test" ->
            solveA Input.testInput

        "input" ->
            solveA Input.input

        _ ->
            "supported args are \"test\" or \"input\""


partB : String -> String
partB inputType =
    case inputType of
        "test" ->
            solveB Input.testInput

        "input" ->
            solveB Input.input

        _ ->
            "supported args are \"test\" or \"input\""


