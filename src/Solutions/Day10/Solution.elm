module Solutions.Day10.Solution exposing (..)

{-| 
-}


import Solutions.Day10.Input as Input
import List.Extra exposing (cycle)

type alias State =
    { x : Int
    , cycle : Int
    , signalStrength : Int
    , screen : List String
    }


intitalState : State
intitalState =
    { x = 1
    , signalStrength = 0
    , cycle = 1
    , screen = ["", "", "", "", "", ""] }


drawScreen : State -> String
drawScreen { screen } =
    String.join "\n" screen


add : Int -> State -> State
add n { x, cycle, signalStrength, screen } =
    let
        newSignal =
            if ((modBy 40 cycle) == 20 && cycle <= 220) then
                signalStrength + (x * cycle)
            else
                signalStrength

        xCoord = 
            (modBy 40 (cycle - 1))

        currentRowNumber =
            (cycle - 1) // 40

        currentRow =
            Maybe.withDefault "" (List.Extra.getAt currentRowNumber screen)

        newPixel =
            if List.member xCoord [ x - 1, x, x + 1 ] then
                "#"
            else
                "."

        newRow =
            currentRow ++ newPixel
    in

    { signalStrength = newSignal
    , cycle = cycle + 1
    , x = x + n
    , screen = List.Extra.setAt currentRowNumber newRow screen }


parseLine : String -> List (State -> State)
parseLine line =
    if line == "noop" then
        [ add 0 ]
    else
        case String.split " " line of
            [ "addx", x ] ->
                case String.toInt x of
                    Just toAdd ->
                        [ add 0, add toAdd ]
                    Nothing ->
                        []
            _ ->
                []


solveA : String -> String
solveA input =
    String.lines input
        |> List.map parseLine
        |> List.concatMap identity
        |> List.foldl (\fn state -> fn state) intitalState
        |> .signalStrength
        |> String.fromInt


solveB : String -> String
solveB input =
    String.lines input
        |> List.map parseLine
        |> List.concatMap identity
        |> List.foldl (\fn state -> fn state) intitalState  
        |> drawScreen


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


