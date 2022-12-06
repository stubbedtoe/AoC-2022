module Solutions.Day6.Solution exposing (..)

{-| 
-}


import Solutions.Day6.Input as Input
import Set
import List.Extra exposing (Step(..))

containsRepeatedChar : List Char -> Bool
containsRepeatedChar chars =
    let
        set =
            Set.fromList chars
    in
    Set.size set /= List.length chars


solveInner : List Char -> Int -> Int -> Int
solveInner chars index take =
    if (containsRepeatedChar (List.take take chars)) then
        case (List.tail chars) of
            Just rest ->
                solveInner rest (index + 1) take
            Nothing ->
                index
    else
        index

solveA : String -> String
solveA input =
    solveInner (String.toList input) 4 4
        |> String.fromInt


solveB : String -> String
solveB input =
    solveInner (String.toList input) 14 14
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


