module Solutions.Day6.Solution exposing (..)

{-| 
-}


import Solutions.Day6.Input as Input
import Set
import List.Extra exposing (Step(..))
import Solutions.Day1.Solution exposing (solveA)

containsRepeatedChar : List Char -> Bool
containsRepeatedChar chars =
    let
        set =
            Set.fromList chars
    in
    Set.size set /= List.length chars


solveBInner : List Char -> Int -> Int
solveBInner chars index =
    if (containsRepeatedChar (List.take 14 chars)) then
        let
            rest =
                (List.tail chars)
        in
        case rest of
            Just r ->
                solveBInner r (index + 1)
            Nothing ->
                index
    else
        index

solveAInner : List Char -> Int -> Int
solveAInner chars index =
    if (containsRepeatedChar (List.take 4 chars)) then
        let
            rest =
                (List.tail chars)
        in
        case rest of
            Just r ->
                solveAInner r (index + 1)
            Nothing ->
                index
    else
        index

solveA : String -> String
solveA input =
    solveAInner (String.toList input) 4
        |> String.fromInt


solveB : String -> String
solveB input =
    solveBInner (String.toList input) 14
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


