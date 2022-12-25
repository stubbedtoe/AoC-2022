module Solutions.Day18.Solution exposing (..)

{-| 
-}


import Solutions.Day18.Input as Input
import Parser exposing (Parser, succeed, symbol, int, (|.), (|=))
import Dict exposing (Dict)
import List.Extra

type alias Position3D = 
    { x : Int
    , y : Int
    , z : Int
    }

type alias AllCubes = Dict String Position3D
type alias CubeIds = List String

type alias ContainingCube = 
    { minX : Int
    , maxX : Int
    , minY : Int
    , maxY : Int
    , minZ : Int
    , maxZ : Int
    }

addTouchingExteriorAir : List Position3D -> List Position3D -> List Position3D
addTouchingExteriorAir knownExteriors allAir =
    let
        touchingKnown =
            List.filter (\air ->
                    let
                        adjacent =
                            getAdjacentPositions air
                        
                    in
                    if (List.Extra.notMember air knownExteriors) && (List.any (\adj -> List.member adj knownExteriors) adjacent) then
                        True
                    else
                        False
                ) allAir
    in
    case touchingKnown of
        [] ->
            knownExteriors
        _ ->
            addTouchingExteriorAir (knownExteriors ++ touchingKnown) allAir

positionToString : Position3D -> String
positionToString { x, y, z } =
    List.map String.fromInt [x, y, z] |> String.join "-"

constructContainingCube : List Position3D -> ContainingCube
constructContainingCube allPositions =
    let
        allX = 
            List.map .x allPositions
        allY =
            List.map .y allPositions
        allZ = 
            List.map .z allPositions

    in
    { minX = Maybe.withDefault 0 (List.minimum allX)
    , maxX = Maybe.withDefault 0 (List.maximum allX)
    , minY = Maybe.withDefault 0 (List.minimum allY)
    , maxY = Maybe.withDefault 0 (List.maximum allY)
    , minZ = Maybe.withDefault 0 (List.minimum allZ)
    , maxZ = Maybe.withDefault 0 (List.maximum allZ)
    }


parseLine : Parser (String, Position3D)
parseLine =
    succeed (\x y z -> let pos = {x=x, y=y, z=z} in (positionToString pos, pos) )
        |= int
        |. symbol ","
        |= int
        |. symbol ","
        |= int


getAdjacentPositions : Position3D -> List Position3D
getAdjacentPositions pos =
    [ { pos | x = pos.x + 1 }
    , { pos | x = pos.x - 1 }
    , { pos | y = pos.y - 1 }
    , { pos | y = pos.y + 1 }
    , { pos | z = pos.z - 1 }
    , { pos | z = pos.z + 1 }
    ]

getAdjacentKeys : Position3D -> List String
getAdjacentKeys pos =
    List.map 
        positionToString 
        (getAdjacentPositions pos)

getAllAirCubesAtZ : ContainingCube -> List Position3D -> Int -> List Position3D
getAllAirCubesAtZ { minX, maxX, minY, maxY } solidCubesAtZ z =
    List.concatMap
        (\x -> 
            List.map 
                (\y -> {x=x, y=y, z=z})
                (List.range minY maxY)
        )
        (List.range minX maxX)
        |> List.filter (\pos -> List.Extra.notMember pos solidCubesAtZ)

getAllSolidCubesAtZ : List Position3D -> Int -> List Position3D
getAllSolidCubesAtZ solidCubes zSlice =
    List.filter (\{ z } -> z == zSlice) solidCubes



getAllAirCubes : List Position3D -> ContainingCube -> List Position3D
getAllAirCubes allSolidCubes container =
    List.foldl (\z acc ->
        let
            allSolidCubesAtZ =
                getAllSolidCubesAtZ allSolidCubes z
            allAirCubesAtZ =
                getAllAirCubesAtZ container allSolidCubesAtZ z
        in
        acc ++ allAirCubesAtZ
    )
    []
    (List.range container.minZ container.maxZ)

type alias CounterAccum =
    { count : Int
    , ids : CubeIds
    }


countSurfaceArea : String -> Position3D -> CounterAccum -> CounterAccum
countSurfaceArea _ pos acc =
    let 
        adjacentCubes =
            List.foldl (\ajKey n -> if List.member ajKey acc.ids then n - 1 else n ) 6 (getAdjacentKeys pos)
    in
    { acc | count = acc.count + adjacentCubes }


solveA : String -> String
solveA input =
    let
        lookup =
            String.lines input
                |> List.map (Parser.run parseLine)
                |> List.filterMap Result.toMaybe
                |> Dict.fromList
        allCubes =
            Dict.keys lookup
    in
    Dict.foldl countSurfaceArea {count = 0, ids = allCubes} lookup
        |> .count
        |> String.fromInt


solveB : String -> String
solveB input =
    let
        lookup =
            String.lines input
                |> List.map (Parser.run parseLine)
                |> List.filterMap Result.toMaybe
                |> Dict.fromList
        allSolidCubes =
            Dict.values lookup
        container =
            constructContainingCube allSolidCubes

        { minX, maxX, minY, maxY, minZ, maxZ } =
            container
        allAirCubes =
            getAllAirCubes allSolidCubes container

        knownExteriorAir =
            List.filter (\{ x, y, z } -> (x == minX) || (x == maxX) || (y == minY) || (y == maxY) || (z == minZ) || (z == maxZ)) allAirCubes

        allExteriorAir =
            addTouchingExteriorAir knownExteriorAir allAirCubes

        allAirPockets =
            List.filter (\air -> List.Extra.notMember air allExteriorAir) allAirCubes

        airPocketLookup =
            List.map (\pos -> (positionToString pos, pos)) allAirPockets
                |> Dict.fromList
        airPocketSurfaceArea =
            Dict.foldl countSurfaceArea {count = 0, ids = (Dict.keys airPocketLookup)} airPocketLookup
                |> .count
        
        solidSurfaceArea =
            Dict.foldl countSurfaceArea {count = 0, ids = (Dict.keys lookup)} lookup
                |> .count

        difference =
            solidSurfaceArea - airPocketSurfaceArea


    in
    String.fromInt difference


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


