module Solutions.Day5.Solution exposing (..)

{-| 
-}


import Solutions.Day5.Input as Input
import Parser exposing (Parser, symbol, spaces, succeed, int, (|.), (|=))
import Array exposing (Array)
import Parser exposing (number)

type alias Instruction = 
    { number : Int
    , from : Int
    , to : Int
    }

type alias Crate =
    List Char

type alias Crates =
    Array Crate


type Part = A | B


getResult : Crates -> String
getResult crates =
    Array.toList crates
        |> List.map (\crate -> Maybe.withDefault ' ' (List.head crate))
        |> String.fromList


executeInstruction : Part -> Instruction -> Crates -> Crates
executeInstruction part { number, from, to } crates  =
    let
        indexFrom
            = from - 1
        indexTo
            = to - 1

        fromCrate
            = Array.get indexFrom crates
                |> Maybe.withDefault []
        toCrate
            = Array.get indexTo crates
                |> Maybe.withDefault []
        boxesToMove =
            case part of
                A ->
                    List.take number fromCrate
                    |> List.reverse
                B ->
                    List.take number fromCrate

        newFrom =
            List.drop number fromCrate
        
        newTo =
            (boxesToMove ++ toCrate)
            
    in
    Array.set indexFrom newFrom crates
        |> Array.set indexTo newTo
    


parseInstruction : Parser Instruction
parseInstruction =
    succeed Instruction
        |. symbol "move"
        |. spaces
        |= int
        |. spaces
        |. symbol "from"
        |. spaces
        |= int
        |. spaces
        |. symbol "to"
        |. spaces
        |= int


runInstructionsOnCrates : Part -> (Crates, List Instruction) -> Crates
runInstructionsOnCrates part (crates, instructions) =
    List.foldl
        (executeInstruction part)
        crates
        instructions

{- 
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

-}

testCrates : Crates
testCrates =
    Array.fromList [
        ['N', 'Z']
        , ['D', 'C', 'M']
        , ['P']
    ]

{- 
    [P]                 [Q]     [T]
[F] [N]             [P] [L]     [M]
[H] [T] [H]         [M] [H]     [Z]
[M] [C] [P]     [Q] [R] [C]     [J]
[T] [J] [M] [F] [L] [G] [R]     [Q]
[V] [G] [D] [V] [G] [D] [N] [W] [L]
[L] [Q] [S] [B] [H] [B] [M] [L] [D]
[D] [H] [R] [L] [N] [W] [G] [C] [R]

-}

inputCrates : Crates
inputCrates =
    Array.fromList [
        ['F', 'H', 'M', 'T', 'V', 'L', 'D']
        , ['P', 'N', 'T', 'C', 'J', 'G', 'Q', 'H']
        , ['H', 'P', 'M', 'D', 'S', 'R']
        , ['F', 'V', 'B', 'L']
        , ['Q', 'L', 'G', 'H', 'N']
        , ['P', 'M', 'R', 'G', 'D', 'B', 'W']
        , ['Q', 'L', 'H', 'C', 'R', 'N', 'M', 'G']
        , ['W', 'L', 'C']
        , ['T', 'M', 'Z', 'J', 'Q', 'L', 'D', 'R']
    ]


parseTestInput : (Crates, List Instruction)
parseTestInput =
    (testCrates
    , String.lines Input.testInput
        |> List.drop 5
        |> List.filterMap (\line -> Parser.run parseInstruction line |> Result.toMaybe))


parseInput : (Crates, List Instruction)
parseInput =
    (inputCrates
    , String.lines Input.input
        |> List.drop 10
        |> List.filterMap (\line -> Parser.run parseInstruction line |> Result.toMaybe))


solveA : String -> String
solveA input =
    "implement partA here"


solveB : String -> String
solveB input =
    "implement partB here"


partA : String -> String
partA inputType =
    case inputType of
        "test" ->
            parseTestInput
                |> runInstructionsOnCrates A
                |> getResult

        "input" ->
            parseInput
                |> runInstructionsOnCrates A
                |> getResult

        _ ->
            "supported args are \"test\" or \"input\""


partB : String -> String
partB inputType =
    case inputType of
        "test" ->
            parseTestInput
                |> runInstructionsOnCrates B
                |> getResult

        "input" ->
            parseInput
                |> runInstructionsOnCrates B
                |> getResult

        _ ->
            "supported args are \"test\" or \"input\""


