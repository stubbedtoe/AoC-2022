module Solutions.Day15.Solution exposing (..)

{-| 
-}


import Solutions.Day15.Input as Input
import Parser exposing (Parser, oneOf, spaces, succeed, keyword, (|=), (|.), int, symbol)
import Set
import List.Extra
import List.Extra exposing (Step(..))

type alias Point =
    ( Int
    , Int )

type alias Beacon = Point

type alias Sensor =
    { location : Point
    , closestBeacon : Beacon
    , distanceToBeacon : Int
    }


edgesAroundSensorAtRow : Int -> Sensor -> List Point
edgesAroundSensorAtRow row { location , distanceToBeacon } =
    let
        (x, y) =
            location

        absYDistance =
            abs (row - y)

        absXDistance =
            abs (distanceToBeacon - absYDistance)

        distanceFromRow =
            distance location (x - absXDistance, row)
    in

    if distanceFromRow == distanceToBeacon then

        if absXDistance == 0 then
            -- straight up or down only
            [(x, row)]
        else
            -- left and right
            [(x - absXDistance, row), (x + absXDistance, row)]
    else
        []


allPointsOnACircumference : Int -> List Sensor -> List Point
allPointsOnACircumference row sensors =
    List.concatMap (edgesAroundSensorAtRow row) sensors
        |> Set.fromList
        |> Set.toList

allBeacons : List Sensor -> List Point
allBeacons =
    List.map .closestBeacon

listOfPointsAtRow : Int -> List Point -> List Point
listOfPointsAtRow row sliceAtRow =
    case sliceAtRow of
        (x1, _) :: (x2, _) :: _ ->
            if x1 < x2 then
                List.map (\x -> (x, row)) (List.range x1 x2)
            else
                List.map (\x -> (x, row)) (List.range x2 x1)
        p :: [] ->
            [ p ]
        _ ->
            []

allPointsWithoutBeaconsAtRow : Int -> List Sensor -> List Point
allPointsWithoutBeaconsAtRow row sensors =
    let
        points =
            List.map (edgesAroundSensorAtRow row) sensors
                |> List.concatMap (listOfPointsAtRow row)
                |> Set.fromList
        beacons =
            allBeacons sensors |> Set.fromList
    in
    Set.diff points beacons
        |> Set.toList

numberOfPointsInRowWithoutBeacons : Int -> List Point -> Int
numberOfPointsInRowWithoutBeacons row =
    List.foldl (\(_, y) count -> if y == row then count + 1 else count) 0


distance : Point -> Point -> Int
distance (x1, y1) (x2, y2) =
    (abs (x1 - x2)) + (abs (y1 - y2))

myInt : Parser Int
myInt =
  oneOf
    [ succeed negate
        |. symbol "-"
        |= int
    , int
    ]

parsePoint : Parser Point
parsePoint =
    succeed (\x y -> (x, y))
        |. symbol "x"
        |. symbol "="
        |= myInt
        |. symbol ","
        |. spaces
        |. symbol "y"
        |. symbol "="
        |= myInt


constructSensor : Point -> Beacon -> Sensor
constructSensor location beacon =
    { location = location
    , closestBeacon = beacon
    , distanceToBeacon = distance location beacon
    }

parseSensor : Parser Sensor
parseSensor =
    succeed constructSensor
        |. keyword "Sensor"
        |. spaces
        |. keyword "at"
        |. spaces
        |= parsePoint
        |. symbol ":"
        |. spaces
        |. keyword "closest"
        |. spaces
        |. keyword "beacon"
        |. spaces
        |. keyword "is"
        |. spaces
        |. keyword "at"
        |. spaces
        |= parsePoint
        
parseInput : String -> List Sensor
parseInput =
    String.lines >> List.map (Parser.run parseSensor) >> List.filterMap Result.toMaybe

solveA : String -> Int -> String
solveA input row =
    parseInput input
        |> allPointsWithoutBeaconsAtRow row
        |> numberOfPointsInRowWithoutBeacons row
        |> String.fromInt


partBAnswer : Point -> String
partBAnswer (x, y) =
    (x * 4000000) + y
        |> String.fromInt

solveB : String -> Int -> String
solveB input max =
    let
        sensors =
            parseInput input
    in
    List.Extra.stoppableFoldl (\row _ ->
        let
            filledAtRow =

                List.foldl (\sensor filled ->
                    case edgesAroundSensorAtRow row sensor of
                        (x, _) :: [] ->
                            if x >= 0 && x <= max then
                                Set.insert x filled
                            else
                                filled
                        (x1, _) :: (x2, _) :: [] ->
                            List.foldl (\x acc ->
                                if x >= 0 && x <= max then
                                    Set.insert x acc
                                else
                                    acc
                                ) filled (List.range x1 x2)
                        _ ->
                            filled
                ) Set.empty sensors
        in
        if (Set.size filledAtRow) == max then
            case (Set.diff (Set.fromList (List.range 0 max)) filledAtRow) |> Set.toList of
                x :: [] ->
                    Stop (partBAnswer (x, row))
                _ ->
                    Continue ""
        else
            Continue ""
    ) "" (List.range 0 max)


partA : String -> String
partA inputType =
    case inputType of
        "test" ->
            solveA Input.testInput 10

        "input" ->
            solveA Input.input 2000000

        _ ->
            "supported args are \"test\" or \"input\""


partB : String -> String
partB inputType =
    case inputType of
        "test" ->
            solveB Input.testInput 20

        "input" ->
            solveB Input.input 4000000

        _ ->
            "supported args are \"test\" or \"input\""


