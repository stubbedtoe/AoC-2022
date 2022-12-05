module Solutions.Day4.Solution exposing (..)

{-| 
-}


import Solutions.Day4.Input as Input
import Parser exposing (Parser, (|.), (|=), succeed, int, symbol)
import Result
import Set exposing (Set)

type alias ElfPair = 
    { lower1 : Int
    , upper1 : Int
    , lower2 : Int
    , upper2 : Int
    }

type alias ElfSets = (Set Int, Set Int)

isSubsetOf : (Set comparable, Set comparable) -> Bool
isSubsetOf (set1, set2) =
    let
        intersection =
            Set.intersect set1 set2
    in
    (Set.diff set1 intersection |> Set.isEmpty) || (Set.diff set2 intersection |> Set.isEmpty)

pairsToSets : ElfPair -> ElfSets
pairsToSets { lower1, upper1, lower2, upper2 } =
    (Set.fromList (List.range lower1 upper1)
    , Set.fromList (List.range lower2 upper2))

overlapAtAll : ElfSets -> Bool
overlapAtAll (set1, set2) =
    Set.intersect set1 set2
        |> Set.size
        |> (<) 0

parseElfPair : Parser ElfPair
parseElfPair =
    succeed ElfPair
        |= int
        |. symbol "-"
        |= int
        |. symbol ","
        |= int
        |. symbol "-"
        |= int


parseLines : String -> List ElfPair
parseLines input =
    String.lines input
        |> List.filterMap (\s -> Parser.run parseElfPair s |> Result.toMaybe)


solveA : String -> String
solveA input =
    parseLines input
    |> List.map pairsToSets
    |> List.filter isSubsetOf
    |> List.length
    |> String.fromInt


solveB : String -> String
solveB input =
    parseLines input
    |> List.map pairsToSets
    |> List.filter overlapAtAll
    |> List.length
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


