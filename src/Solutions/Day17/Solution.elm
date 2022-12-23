module Solutions.Day17.Solution exposing (..)

{-| 
-}


import Solutions.Day17.Input as Input
-- import List.Nonempty as NE exposing (Nonempty)
import Binary exposing (Bits)
import List.Extra
import Solutions.Day15.Solution exposing (distance)
import Array exposing (Array)
import Array.Extra
import Bitwise
import Dict exposing (Dict)

type Rock = Horizontal
    | Cross
    | BackwardL
    | Vertical
    | Square

type Move = Right
    | Left
    | Down

type alias State = 
    { currentRock : Rock
    , distanceFromTip : Int
    , rightFromLeftEdge : Int
    , height : Int
    , settledRocks : Array Int
    , numberFallen : Int
    , seenCombinationsWithHeights : Dict String (Int, Int)
    , gustsBlown : Int
    , allGusts : List Char
    , targetFallenRocks : Int
    , usedPattern : String
    }

type alias BinaryRocks = Array Int

type alias Buffer = Array (Int, Int)


initialiseState : String -> Int -> State
initialiseState input target =
    { currentRock = Horizontal
    , distanceFromTip = -3
    , height = 0
    , settledRocks = Array.fromList [ List.repeat 7 1 |> Binary.fromIntegers >> Binary.toDecimal ]
    , numberFallen = 0
    , rightFromLeftEdge = 2
    , gustsBlown = 0
    , allGusts = String.toList input
    , seenCombinationsWithHeights = Dict.empty
    , targetFallenRocks = target
    , usedPattern = ""
    }

getRock : Int -> Rock
getRock fallen =
    case (modBy 5 fallen) of
        1 ->
            Cross
        2 ->
            BackwardL
        3 ->
            Vertical
        4 ->
            Square
        _ ->
            Horizontal

-- so we know how many rows to compare against
getHeight : Rock -> Int
getHeight rock =
    case rock of
        Horizontal ->
            1
        Cross ->
            3
        BackwardL ->
            3
        Vertical ->
            4
        Square ->
            2

horizontalInts : BinaryRocks
horizontalInts =
    List.map Binary.fromIntegers 
        [ [ 1, 1, 1, 1, 0, 0, 0 ] 
        ]
        |> List.map Binary.toDecimal
        |> Array.fromList

crossInts : BinaryRocks
crossInts =
    List.map Binary.fromIntegers 
        [ [ 0, 1, 0, 0, 0, 0, 0 ]
        , [ 1, 1, 1, 0, 0, 0, 0 ]
        , [ 0, 1, 0, 0, 0, 0, 0 ]
        ]
        |> List.map Binary.toDecimal
        |> Array.fromList

backwardLInts : BinaryRocks
backwardLInts =
    List.map Binary.fromIntegers 
        [ [ 0, 0, 1, 0, 0, 0, 0 ]
        , [ 0, 0, 1, 0, 0, 0, 0 ]
        , [ 1, 1, 1, 0, 0, 0, 0 ]
        ]
        |> List.map Binary.toDecimal
        |> Array.fromList

verticalInts : BinaryRocks
verticalInts =
    List.map Binary.fromIntegers 
        [ [ 1, 0, 0, 0, 0, 0, 0 ]
        , [ 1, 0, 0, 0, 0, 0, 0 ]
        , [ 1, 0, 0, 0, 0, 0, 0 ] 
        , [ 1, 0, 0, 0, 0, 0, 0 ]  
        ]
        |> List.map Binary.toDecimal
        |> Array.fromList

squareInts : BinaryRocks
squareInts =
    List.map Binary.fromIntegers
        [ [ 1, 1, 0, 0, 0, 0, 0 ]
        , [ 1, 1, 0, 0, 0, 0, 0 ]
        ]
        |> List.map Binary.toDecimal
        |> Array.fromList

getRockBinary : State -> BinaryRocks
getRockBinary { currentRock, rightFromLeftEdge } =
    let
        initial =
            case currentRock of
                Horizontal ->
                    horizontalInts
                Cross ->
                    crossInts
                BackwardL ->
                    backwardLInts
                Vertical ->
                    verticalInts
                Square ->
                    squareInts
    in
    Array.map (Bitwise.shiftRightZfBy rightFromLeftEdge) initial

atTheLeftEdge : BinaryRocks -> Bool
atTheLeftEdge binaries =
    Array.toList binaries
        |> List.any (\n -> n >= 64)

atTheRightEdge : BinaryRocks -> Bool
atTheRightEdge binaries =
    Array.toList binaries
        |> List.map (modBy 2)
        |> List.any ((==) 1)

filledByRocks : BinaryRocks -> BinaryRocks
filledByRocks binaries =
    Array.map Binary.fromDecimal binaries
        |> Array.map Binary.toIntegers
        |> Array.map List.sum

noCollisions : BinaryRocks -> Buffer -> Bool
noCollisions rock buffer =
    let
        expectedIncrease =
            filledByRocks rock
        bufferBinary =
            Array.map Tuple.first buffer
        existingFilled =
            filledByRocks bufferBinary
        combinedFilled = 
            Array.Extra.map2 
                Bitwise.xor 
                rock 
                bufferBinary
                |> filledByRocks
        actualIncrease =
            Array.Extra.map2
                (\existing combined -> combined - existing)
                existingFilled
                combinedFilled
    in
    Array.Extra.map2 
        (\expected actual -> expected == actual)
        expectedIncrease
        actualIncrease
        |> Array.toList
        |> List.all identity


shiftLeft : BinaryRocks -> BinaryRocks
shiftLeft =
    Array.map (Bitwise.shiftLeftBy 1)

shiftRight : BinaryRocks -> BinaryRocks
shiftRight =
    Array.map (Bitwise.shiftRightZfBy 1)

canShiftLeft : BinaryRocks -> Buffer -> Bool
canShiftLeft rock buffer =
    if atTheLeftEdge rock then
        False
    else
        noCollisions (shiftLeft rock) buffer


canShiftRight : BinaryRocks -> Buffer -> Bool
canShiftRight rock buffer =
    if atTheRightEdge rock then
        False
    else
        noCollisions (shiftRight rock) buffer

settleRock : State -> State
settleRock state =
    let
        buffer =
            getBuffer Left state
        settled =
            Array.map Tuple.first buffer
        indices =
            Array.map Tuple.second buffer
        rockAsBinary =
            getRockBinary state
        combinedRocks =
            Array.Extra.map2 Bitwise.xor settled rockAsBinary

        addedHeight =
            (getHeight state.currentRock) - state.distanceFromTip

        combinedSlice =
            Array.Extra.zip combinedRocks indices

        settledWithAddedSpace =
            if addedHeight > 0 then
                Array.Extra.resizerRepeat ((Array.length state.settledRocks) + addedHeight) 0 state.settledRocks
            else
                state.settledRocks
       
        newSettled =
            Array.foldl 
                (\(binary, index) settledRocks -> 
                    Array.set index binary settledRocks
                ) 
                settledWithAddedSpace
                combinedSlice
        
        newHeight =
            if addedHeight > 0 then
                state.height + addedHeight
            else
                state.height

        newFallenRocks =
            state.numberFallen + 1

    --     key =
    --         (String.fromInt (modBy 5 newFallenRocks)) ++ "-" ++ (String.fromInt (modBy (List.length state.allGusts) state.gustsBlown))
        
    --     value =
    --         (newHeight, newFallenRocks)
    in
    -- case Dict.get key state.seenCombinationsWithHeights of
    --     Just (previousHeight, afterNrocks) ->
    --         let
    --             patternHeight =
    --                 newHeight - previousHeight
    --             numRocksInPattern =
    --                 newFallenRocks - afterNrocks
    --             numPatternsRequired =
    --                 (state.targetFallenRocks - newFallenRocks) // numRocksInPattern
    --             fallenRocksWithAddedRocks =
    --                 newFallenRocks + (numRocksInPattern * numPatternsRequired)
    --             heightWithAddedRocks =
    --                 newHeight + (patternHeight * numPatternsRequired)
    --         in
    --         { state | currentRock = getRock newFallenRocks
    --         , numberFallen = fallenRocksWithAddedRocks
    --         , distanceFromTip = -3
    --         , height = heightWithAddedRocks
    --         , settledRocks = newSettled
    --         , rightFromLeftEdge = 2
    --         , usedPattern = "Pattern at key " ++ key ++ " = height: " ++ (String.fromInt patternHeight) ++ ", with " ++ (String.fromInt numRocksInPattern) ++ " rocks" 
    --         }
    --     Nothing ->
            { state | currentRock = getRock newFallenRocks
            , numberFallen = newFallenRocks
            , distanceFromTip = -3
            , height = newHeight
            , settledRocks = newSettled
            , rightFromLeftEdge = 2
            --, seenCombinationsWithHeights = Dict.insert key value state.seenCombinationsWithHeights
            }

getBuffer : Move -> State -> Buffer
getBuffer direction { settledRocks, currentRock, distanceFromTip } =
    let
        rockHeight =
            getHeight currentRock
        start = 
            case direction of
                Down ->
                    (distanceFromTip - rockHeight) + 1
                _ ->
                    distanceFromTip - rockHeight
        end =
            start + rockHeight

        settledWithAddedSpace =
            if start < 0 then
                Array.Extra.resizerRepeat ((abs start) + (Array.length settledRocks)) 0 settledRocks
            else
                settledRocks
        rocks =
            if start < 0 then
                Array.slice 0 (end - start) settledWithAddedSpace
            else
                Array.slice start end settledWithAddedSpace
        indices =
            if start < 0 then
                List.range 0 (end - start - 1)
                    |> Array.fromList
            else
                List.range start (end - 1)
                    |> Array.fromList
    in
    Array.Extra.zip rocks indices

move : Move -> State -> State
move direction state =
    let
        rockBinary =
            getRockBinary state
    in
    if state.distanceFromTip < 0 then
        case direction of
            Down ->
                { state | distanceFromTip = state.distanceFromTip + 1 }
            Left ->
                if atTheLeftEdge rockBinary then
                    state
                else
                    { state | rightFromLeftEdge = state.rightFromLeftEdge - 1 }
            Right ->
                if atTheRightEdge rockBinary then
                    state
                else
                    { state | rightFromLeftEdge = state.rightFromLeftEdge + 1 }
    else if state.distanceFromTip == 0 then
        case direction of
            Down ->
                if noCollisions rockBinary (getBuffer direction state) then
                    { state | distanceFromTip = state.distanceFromTip + 1 }
                else
                    settleRock state
            Left ->
                if atTheLeftEdge rockBinary then
                    state
                else
                    { state | rightFromLeftEdge = state.rightFromLeftEdge - 1 }
            Right ->
                if atTheRightEdge rockBinary then
                    state
                else
                    { state | rightFromLeftEdge = state.rightFromLeftEdge + 1 }
    else
        case direction of
            Down ->
                if noCollisions rockBinary (getBuffer direction state) then
                    { state | distanceFromTip = state.distanceFromTip + 1 }
                else
                    settleRock state
            Right ->
                if canShiftRight rockBinary (getBuffer direction state) then
                    { state | rightFromLeftEdge = state.rightFromLeftEdge + 1 }
                else
                    state
            Left ->
                if canShiftLeft rockBinary (getBuffer direction state) then
                    { state | rightFromLeftEdge = state.rightFromLeftEdge - 1 }
                else
                    state
                

consumeChar : Char -> State  -> State
consumeChar c state =
    case c of
        '<' ->
            move Left { state | gustsBlown = state.gustsBlown + 1 } |> move Down
        '>' ->
            move Right { state | gustsBlown = state.gustsBlown + 1 } |> move Down
        _ ->
            state


recurse : State -> State
recurse state =
    if state.numberFallen == state.targetFallenRocks then
        state

    else
        case List.Extra.getAt (modBy (List.length state.allGusts) state.gustsBlown) state.allGusts of
            Just char ->
                recurse (consumeChar char state)
            Nothing ->
                state


stateToString : State -> String
stateToString state =
    -- "Pattern height: " ++ (String.fromInt pattern.patternHeight)
    --     ++ " | Pattern found after " ++ (String.fromInt pattern.foundAtNumberFallen) ++ " rocks fallen and "
    --     ++ (String.fromInt pattern.foundAtGustsBlown) ++ " gusts blown."
    Array.foldl (\line str ->
        let
            nextLine =
                List.foldl (\cell s ->
                        case cell of
                            0 ->
                                s ++ "."
                            _ ->
                                s ++ "#"
                    ) 
                    ""
                    (Binary.fromDecimal line |> Binary.ensureSize 7 |> Binary.toIntegers )
        in
        str ++ "\n" ++ nextLine
    ) state.usedPattern state.settledRocks

solveA : String -> String
solveA input =
    let
        finalState = 
            recurse (initialiseState input 2022)
    in
    String.fromInt finalState.height
    -- stateToString finalState


solveB : String -> String
solveB input =
    let
        finalState = 
            recurse (initialiseState input 1000000000000)
    in
    String.fromInt finalState.height


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


