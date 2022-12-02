module Solutions.Day1.Solution exposing (..)

{-| 
-}


import Solutions.Day1.Input as Input
import Utils

solveA : String -> String
solveA input =
    Utils.splitOnEmptyLine (String.lines input)
        |> List.map (List.filterMap String.toInt)
        |> List.map List.sum
        |> List.maximum
        |> Maybe.withDefault 0
        |> String.fromInt


solveB : String -> String
solveB input =
    let
        sortedCalories =
            Utils.splitOnEmptyLine (String.lines input)
                |> List.map (List.filterMap String.toInt)
                |> List.map List.sum
                |> List.sort

        numTodrop =
            (List.length sortedCalories) - 3

    in
    List.drop numTodrop sortedCalories
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


