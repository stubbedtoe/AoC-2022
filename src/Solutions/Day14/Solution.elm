module Solutions.Day14.Solution exposing (..)

{-| 
-}


import Solutions.Day14.Input as Input

import Dict exposing (Dict)
import Parser exposing (Parser, succeed, (|.), (|=), int, symbol)
import Set exposing (Set)

type Solid = Rock | Sand
type Wall = Horizontal Int (Int, Int) | Vertical Int (Int, Int)

type alias Location = (Int, Int)

type alias Cave = Dict Location Solid

type alias Corner = String

type Part = A | B

type alias RockSegments = Set Location
type alias SandSegments = Set Location


type alias Edges =
    { left : Int
    , right : Int
    , depth : Int }


locationIsOccupied2 : RockSegments -> SandSegments -> Int -> Location -> Bool
locationIsOccupied2 rock sand depth particle =
    if (Set.member particle rock) || (Set.member particle sand) then
        True
    else
        (Tuple.second particle) == depth

locationIsOccupied : Cave -> Edges -> Part -> Location -> Bool
locationIsOccupied cave { depth } part (x, y) =
    case Dict.get (x, y) cave of
        Just _ ->
            True
        Nothing ->
            case part of
                A -> False
                B -> y == depth


dropSand2 : Int -> SandSegments -> RockSegments -> String
dropSand2  =
    fall2 (500, -1)


fall2 : Location -> Int -> SandSegments -> RockSegments -> String
fall2 (x, y) depth sand rock =
    let
        down =
            (x, y + 1)
        diagonalLeft =
            (x - 1,  y + 1)
        diagonalRight =
            (x + 1,  y + 1)
    in
    if locationIsOccupied2 rock sand depth down then
        if (y == -1) then
            -- entrance blocked up
            String.fromInt (Set.size sand)
        else 
            if locationIsOccupied2 rock sand depth diagonalLeft then
                if locationIsOccupied2 rock sand depth diagonalRight then
                    -- the sand has nowhere to go so place it in the intial location
                    dropSand2 depth (Set.insert (x, y) sand) rock
                else
                    fall2 diagonalRight depth sand rock
            else
                fall2 diagonalLeft depth sand rock
    else
        fall2 down depth sand rock
    


fall : Part -> Edges -> Location -> Cave -> String
fall part edges (x, y) cave =
    let
        isPartA =
            case part of
                A -> True
                B -> False
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


constructCave2 : String -> RockSegments
constructCave2 input =
    String.lines input
        |> List.map parseLine
        |> List.map createWallsFromCorners
        |> List.concatMap identity
        |> List.map constructWall
        |> List.concatMap identity
        |> Set.fromList


filledCave : Part -> Cave -> String
filledCave part cave =
    dropSand part (findEdges part cave) cave

filledCave2 : RockSegments -> String
filledCave2 rock =
    let
        depth =
            Set.toList rock
                |> List.map Tuple.second
                |> List.maximum
                |> Maybe.withDefault 0
                |> (+) 2
    in
    dropSand2 depth Set.empty rock

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

extremeFromSet : Set Location -> (Location -> Int) -> (List Int -> Maybe Int) -> Int
extremeFromSet set pick extreme =
    Set.toList set
        |> List.map pick
        |> extreme
        |> Maybe.withDefault 0

max : Int -> Int -> Int
max a b =
    if a > b then
        a
    else
        b

min : Int -> Int -> Int
min a b =
    if a < b then
        a
    else
        b

visualiseCave2 : RockSegments -> SandSegments -> String
visualiseCave2 rock sand =
    let
        rockDepth =
            extremeFromSet rock Tuple.second List.maximum
        sandDepth =
            extremeFromSet sand Tuple.second List.maximum
        sandLeft =
            extremeFromSet sand Tuple.first List.minimum
        rockLeft =
            extremeFromSet rock Tuple.first List.minimum
        sandRight =
            extremeFromSet sand Tuple.first List.maximum
        rockRight =
            extremeFromSet rock Tuple.first List.maximum
        depth =
            if Set.isEmpty sand then
                rockDepth
            else
                max sandDepth rockDepth
        left =
            if Set.isEmpty sand then
                rockLeft
            else
                min sandLeft rockLeft
        right =
            if Set.isEmpty sand then
                rockRight
            else
                max sandRight rockRight
    in
    List.map (\y ->
        List.foldl (\x s ->
            if Set.member (x, y) sand then
                s ++ "o"
            else if Set.member (x, y) rock then
                s ++ "#"
            else
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
    filledCave2 (constructCave2 input)


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


