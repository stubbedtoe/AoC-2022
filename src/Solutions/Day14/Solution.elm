module Solutions.Day14.Solution exposing (..)

{-| 
-}


import Solutions.Day14.Input as Input

import Dict exposing (Dict)
import Parser exposing (Parser, succeed, (|.), (|=), int, symbol)

type Solid = Rock | Sand
type Wall = Horizontal Int (Int, Int) | Vertical Int (Int, Int)

type alias Location = (Int, Int)

type alias Cave = Dict Location Solid

type alias Corner = String

type Part = A | B


type alias Edges =
    { left : Int
    , right : Int
    , depth : Int }


locationIsOccupied : Cave -> Edges -> Part -> Location -> Bool
locationIsOccupied cave { depth } part (x, y) =
    case Dict.get (x, y) cave of
        Just _ ->
            True
        Nothing ->
            case part of
                A -> False
                B -> y == depth


fall : Part -> Edges -> Location -> Cave -> String
fall part edges (x, y) cave =
    let
        isPartA =
            case part of
                A -> True
                B -> False
        isPartB =
            not isPartA
    in

    case edges of 
        { left, right, depth } ->
            if isPartA && ((y + 1) > depth) then
                String.fromInt (countSand cave)
            else
                let
                    down =
                        (x, y + 1)
                in

                if locationIsOccupied cave edges part down then
                    if (y == -1) then
                        -- entrance blocked up
                        String.fromInt (countSand cave)
                    else if isPartA && ((x - 1) < left) then
                        -- running off the left
                        String.fromInt (countSand cave)
                    else
                        let
                            diagonalLeft =
                                (x - 1, y + 1)
                        in

                        if locationIsOccupied cave edges part diagonalLeft then
                            if isPartA && ((x + 1) > right) then
                                -- running off the right
                                String.fromInt (countSand cave)
                            else
                                let
                                    diagonalRight =
                                        (x + 1, y + 1)
                                in
                                if locationIsOccupied cave edges part diagonalRight then
                                    let
                                        -- the sand has nowhere to go so place it in the first location
                                        newCave =
                                            addSand cave (x, y)
                                    in
                                    -- start the cycle again
                                    dropSand part edges newCave
                                else
                                    -- drop it diagonal right
                                    fall part edges diagonalRight cave
                            else
                                -- drop it diagonal left
                                fall part edges diagonalLeft cave
                        else
                            -- drop it straight down
                            fall part edges down cave


dropSand : Part -> Edges -> Cave -> String
dropSand part edges =
    fall part edges (500, -1)


parseLine : String -> List Corner
parseLine =
    String.split " -> "

parseCorner : Parser Location
parseCorner =
    succeed (\y x -> (x, y))
        |= int
        |. symbol ","
        |= int


runCornerParser : Corner -> Maybe Location
runCornerParser corner =
    Parser.run parseCorner corner
        |> Result.toMaybe


constructWall : Wall -> List Location
constructWall wall =
    case wall of
        Horizontal y (x1, x2) ->
            List.range x1 x2
                |> List.map (\x -> (x, y))
        Vertical x (y1, y2) ->
            List.range y1 y2
                |> List.map (\y -> (x, y))

addRock : Location -> Cave -> Cave
addRock loc cave =
    Dict.insert loc Rock cave

addSand : Cave -> Location -> Cave
addSand cave loc =
    Dict.insert loc Sand cave


createWallsFromCorners : List Corner -> List Wall
createWallsFromCorners corners =
    case corners of
        c1 :: c2 :: rest ->
            case parseCorners c1 c2 of
                Just wall ->
                    wall :: (createWallsFromCorners (c2 :: rest))
                Nothing ->
                    createWallsFromCorners (c2 :: rest)
        _ ->
            []

parseCorners : Corner -> Corner -> Maybe Wall
parseCorners c1 c2 =
    Maybe.map2 (\(x1, y1) (x2, y2) -> 
        if x1 == x2 then
            case List.sort [y1, y2] of
                [lowY, highY] ->
                    Horizontal x1 (lowY, highY)
                _ ->
                    Horizontal x1 (y1, y2)
        else
            case List.sort [x1, x2] of
                [lowX, highX] ->
                    Vertical y1 (lowX, highX)
                _ ->
                    Vertical y1 (x1, x2)
    )
    (runCornerParser c1)
    (runCornerParser c2)


countSand : Cave -> Int
countSand cave =
    Dict.values cave
        |> List.foldl (\solid count ->
            case solid of
                Sand ->
                    count + 1
                Rock ->
                    count
            )
            0


findEdges : Part -> Cave -> Edges
findEdges part cave =
    let
        leftEdge =
            Dict.keys cave
                |> List.map Tuple.first
                |> List.minimum
                |> Maybe.withDefault 0
        rightEdge =
            Dict.keys cave
                |> List.map Tuple.first
                |> List.maximum
                |> Maybe.withDefault 0

        depth =
            Dict.keys cave
                |> List.map Tuple.second
                |> List.maximum
                |> Maybe.withDefault 0
    in
    case part of
        A ->
            { left = leftEdge, right = rightEdge, depth = depth }
        B ->
            { left = leftEdge - 1, right = rightEdge + 1, depth = depth + 2 }


constructCave : String -> Cave
constructCave input =
    String.lines input
        |> List.map parseLine
        |> List.map createWallsFromCorners
        |> List.concatMap identity
        |> List.map constructWall
        |> List.concatMap identity
        |> List.foldl addRock Dict.empty


filledCave : Part -> Cave -> String
filledCave part cave =
    dropSand part (findEdges part cave) cave

visualiseCave : Part -> Cave -> String
visualiseCave part cave =
    case findEdges part cave of
        { left, right, depth } ->
            List.map (\y ->
                    List.foldl (\x s ->
                        case part of
                            A ->
                                case Dict.get (x, y) cave of 
                                    Just Sand ->
                                        s ++ "o"
                                    Just Rock ->
                                        s ++ "#"
                                    Nothing ->
                                        s ++ "."

                            B ->
                                if y == depth then
                                    s ++ "#"
                                else
                                    case Dict.get (x, y) cave of 
                                        Just Sand ->
                                            s ++ "o"
                                        Just Rock ->
                                            s ++ "#"
                                        Nothing ->
                                            s ++ "."
                        ) 
                        ""
                        (List.range left right)
                ) 
                (List.range 0 (depth - 1))
                |> String.join "\n"



solveA : String -> String
solveA input =
    let
        cave =
            constructCave input
    in
    filledCave A cave


solveB : String -> String
solveB input =
    let
        cave =
            constructCave input
    in
    filledCave B cave


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


