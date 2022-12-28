module Solutions.Day22.Solution exposing (..)

{-| 
-}


import Solutions.Day22.Input as Input
import Parser exposing (Parser, succeed, loop, Step(..), int, symbol, (|=), oneOf)
import Dict exposing (Dict)
import Utils

type alias Position = (Int, Int)
type Facing = North
    | South
    | East
    | West

type Direction = Left | Right

type Instruction = Move Int | Turn Direction

type alias Tile =
    { position : (Int, Int)
    , north : Maybe (String, Facing)
    , south : Maybe (String, Facing)
    , east : Maybe (String, Facing)
    , west : Maybe (String, Facing)
    }

type alias SideMinMax =
    { minX : Int
    , maxX : Int
    , minY : Int
    , maxY : Int
    }

type alias Map = Dict String Tile

type alias State = 
    { instructions : List Instruction
    , position : Position
    , facing : Facing
    , map : Map
    }

type Side = One | Two | Three | Four | Five | Six

sideToMinMax : Side -> SideMinMax
sideToMinMax side =
    case side of
        One ->
            { minX = 51, maxX = 100, minY = 1, maxY = 50 }
        Two ->
            { minX = 101, maxX = 150, minY = 1, maxY = 50 }
        Three ->
            { minX = 51, maxX = 100, minY = 51, maxY = 100 }
        Four ->
            { minX = 1, maxX = 50, minY = 101, maxY = 150 }
        Five ->
            { minX = 51, maxX = 100, minY = 101, maxY = 150 }
        Six ->
            { minX = 1, maxX = 50, minY = 151, maxY = 200 }

positionToSide : Position -> Side
positionToSide (x, y) =
    if y <= 50 then
        if x <= 100 then
            One
        else
            Two
    else if y <= 100 then
        Three
    else if y <= 150 then
        if x <= 50 then
            Four
        else
            Five
    else
        Six

{- Facing is 0 for right (>), 1 for down (v), 2 for left (<), and 3 for up (^) -}
facingToInt : Facing -> Int
facingToInt facing =
    case facing of
        North ->
            3
        South ->
            1
        East ->
            0
        West ->
            2

turn : Facing -> Direction -> Facing
turn facing direction =
    case facing of
        North ->
            case direction of
                Left ->
                    West
                Right ->
                    East
        South ->
            case direction of
                Left ->
                    East
                Right ->
                    West
        East ->
            case direction of
                Left ->
                    North
                Right ->
                    South
        West ->
            case direction of
                Left ->
                    South
                Right ->
                    North


makeKey : Position -> String
makeKey (x, y) =
    (String.fromInt x) ++ "," ++ (String.fromInt y)


parseLine : Int -> String -> List (Position, Maybe Bool)
parseLine y line =
    String.toList line
        |> List.indexedMap 
            (\x c ->
                let
                    position =
                        (x + 1, y)
                in
                case c of
                    '.' ->
                        (position, Just True)
                    '#' ->
                        (position, Just False)
                    _ ->
                        (position, Nothing)
            )


parseMap : List String -> List (Position, Maybe Bool)
parseMap lines =
    List.indexedMap (\y line -> parseLine (y + 1) line) lines
        |> List.concatMap identity


translatePosition : Position -> Facing -> Position
translatePosition (x, y) facing =
    case facing of
        North ->
            (x, y - 1)
        South ->
            (x, y + 1)
        East ->
            (x + 1, y)
        West ->
            (x - 1, y)


getNextPt1 : Position -> Facing -> List String -> Maybe (String, Facing)
getNextPt1 ((posX, posY) as position) sideFacing emptyTileKeys =
    let
        oldSide =
            positionToSide position
        { minX, maxX, minY, maxY } =
            sideToMinMax oldSide
        newPosition =
            case oldSide of
                One ->
                    if posY == minY && sideFacing == North then
                        (posX, 150)
                    else if posX == minX && sideFacing == West then
                        (150, posY)
                    else
                        translatePosition position sideFacing
                Two ->
                    if posY == minY && sideFacing == North then
                        (posX, 50)
                    else if posX == maxX && sideFacing == East then
                        (51, posY)
                    else if posY == maxY && sideFacing == South then
                        (posX, 1)
                    else
                        translatePosition position sideFacing
                Three ->
                    if posX == minX && sideFacing == West then
                        (100, posY)
                    else if posX == maxX && sideFacing == East then
                        (51, posY)
                    else
                        translatePosition position sideFacing
                Four ->
                    if posY == minY && sideFacing == North then
                        (posX, 200)
                    else if posX == minX && sideFacing == West then
                        (100, posY)
                    else
                        translatePosition position sideFacing
                Five ->
                    if posX == maxX && sideFacing == East then
                        (1, posY)
                    else if posY == maxY && sideFacing == South then
                        (posX, 1)
                    else
                        translatePosition position sideFacing
                Six ->
                    if posX == minX && sideFacing == West then
                        (50, posY)
                    else if posY == maxY && sideFacing == South then
                        (posX, 101)
                    else if posX == maxX && sideFacing == East then
                        (1, posY)
                    else
                        translatePosition position sideFacing
    in
    let
        key =
            makeKey newPosition
    in
    if List.member key emptyTileKeys then
        Just (key, sideFacing)
    else
        Nothing



getNextPt2 : Position -> Facing -> List String -> Maybe (String, Facing)
getNextPt2 ((posX, posY) as position) sideFacing emptyTileKeys =
    let
        oldSide =
            positionToSide position
        { minX, maxX, minY, maxY } =
            sideToMinMax oldSide
        (relativeX, relativeY) =
            (posX - minX, posY - minY) -- can be (0, 0) to (49, 49)
        (newPosition, newFacing) =
            case oldSide of
                One ->
                    if posY == minY && sideFacing == North then
                        ((1, 151 + relativeX) , East)
                    else if posX == minX && sideFacing == West then
                        ((1, 150 - relativeY) , East)
                    else
                        (translatePosition position sideFacing , sideFacing)
                Two ->
                    if posY == minY && sideFacing == North then
                        ((1 + relativeX, 200) , North)
                    else if posX == maxX && sideFacing == East then
                        ((100, 150 - relativeY) , West)
                    else if posY == maxY && sideFacing == South then
                        ((100, 51 + relativeX), West)
                    else
                        (translatePosition position sideFacing , sideFacing)
                Three ->
                    if posX == minX && sideFacing == West then
                        ((1 + relativeY, 101), South)
                    else if posX == maxX && sideFacing == East then
                        ((101 + relativeY, 50), North)
                    else
                        (translatePosition position sideFacing , sideFacing)
                Four ->
                    if posY == minY && sideFacing == North then
                        ((51, 51 + relativeX), East)
                    else if posX == minX && sideFacing == West then
                        ((51, 50 - relativeY), East)
                    else
                        (translatePosition position sideFacing , sideFacing)
                Five ->
                    if posX == maxX && sideFacing == East then
                        ((150, 50 - relativeY), West)
                    else if posY == maxY && sideFacing == South then
                        ((50, 151 + relativeX),  West)
                    else
                        (translatePosition position sideFacing , sideFacing)
                Six ->
                    if posX == minX && sideFacing == West then
                        ((51 + relativeY, 1), South)
                    else if posY == maxY && sideFacing == South then
                        ((101 + relativeX, 1), South)
                    else if posX == maxX && sideFacing == East then
                        ((51 + relativeY, 150), North)
                    else
                        (translatePosition position sideFacing , sideFacing)
    in
    let
        key =
            makeKey newPosition
    in
    if List.member key emptyTileKeys then
        Just (key, newFacing)
    else
        Nothing


constructMap : List String -> (Position -> Facing -> List String -> Maybe (String, Facing)) -> Map
constructMap lines getNext =
    let
        allTiles =
            parseMap lines

        onlyEmptyTiles =
            List.filter 
                (\(_, contents) ->
                    case contents of
                        Just True ->
                            True
                        _ ->
                            False
                )
                allTiles

        emptyTileKeys =
            List.map (\(position, _) -> makeKey position) onlyEmptyTiles

    in
    List.map 
        (\(position, _) ->
            ( makeKey position
            , { position = position
            , north = getNext position North emptyTileKeys
            , south = getNext position South emptyTileKeys
            , east = getNext position East emptyTileKeys
            , west = getNext position West emptyTileKeys
            })
        ) 
        onlyEmptyTiles
        |> Dict.fromList

    

executeInstructions : State -> String
executeInstructions ({ position, facing, instructions, map } as state) =
    case instructions of
        instruction :: rest ->
            case instruction of
                Move n ->
                    if n > 0 then
                        let
                            options =
                                Dict.get (makeKey position) map
                            
                        in
                        case options of
                            Just currentTile ->
                                let
                                    key =
                                        case facing of
                                            North ->
                                                currentTile.north
                                            South ->
                                                currentTile.south
                                            East ->
                                                currentTile.east
                                            West ->
                                                currentTile.west

                                in
                                case key of
                                    Just (newKey, newFacing) ->
                                        case Dict.get newKey map of
                                            Just tile ->
                                                executeInstructions { state
                                                    | instructions = (Move (n - 1)) :: rest
                                                    , position = tile.position
                                                    , facing = newFacing 
                                                    }
                                            Nothing ->
                                                executeInstructions { state | instructions = rest }
                                    Nothing ->
                                        -- skip
                                        executeInstructions { state | instructions = rest }

                            Nothing ->
                                executeInstructions { state | instructions = rest }
                        
                    else
                        executeInstructions { state | instructions = rest }
                
                Turn direction ->
                    executeInstructions { state 
                        | facing = turn facing direction
                        , instructions = rest
                        }

        [] ->
            let
                (x, y) =
                    position
            in
            (y * 1000) + (4 * x) + (facingToInt facing)
                |> String.fromInt

parseInstructions : Parser (List Instruction)
parseInstructions =
    loop [] instructionsHelp

instructionsHelp : List Instruction -> Parser (Step (List Instruction) (List Instruction))
instructionsHelp revInst =
    oneOf
        [ succeed (\n -> Loop ((Move n) :: revInst)) 
            |= int
        , succeed (\_ -> Loop ((Turn Left) :: revInst))
            |= symbol "L"
        , succeed (\_ -> Loop ((Turn Right) :: revInst))
            |= symbol "R"
        , succeed ()
            |> Parser.map (\_ -> Done (List.reverse revInst))
        ]

dummyState : State
dummyState =
    { map = Dict.empty
    , instructions = []
    , position = (-1, -1)
    , facing = East
    }

solveA : String -> String
solveA input =
    let
        splitOnEmpty =
            Utils.splitOnEmptyLine (String.lines input)
    in
    case splitOnEmpty of
        mapLines :: instructionLines :: [] ->
            let 
                map =
                    constructMap mapLines getNextPt1
                instructionLine =
                    String.join "\n" instructionLines
                parsedInstructions = 
                    Parser.run parseInstructions instructionLine
                        |> Result.toMaybe
            in
            case parsedInstructions of
                Just instructions ->
                    executeInstructions { position = (51, 1)
                    , map = map
                    , facing = East
                    , instructions = instructions }
                Nothing ->
                    "error parsing instructions"
                    -- dummyState
        _ ->
            "error splitting input string"
            -- dummyState


solveB : String -> String
solveB input =
    let
        splitOnEmpty =
            Utils.splitOnEmptyLine (String.lines input)
    in
    case splitOnEmpty of
        mapLines :: instructionLines :: [] ->
            let 
                map =
                    constructMap mapLines getNextPt2
                instructionLine =
                    String.join "\n" instructionLines
                parsedInstructions = 
                    Parser.run parseInstructions instructionLine
                        |> Result.toMaybe
                
            in
            case parsedInstructions of
                Just instructions ->
                    executeInstructions { position = (51, 1)
                    , map = map
                    , facing = East
                    , instructions = instructions }
                Nothing ->
                    "error parsing instructions"
                    -- dummyState
        _ ->
            "error splitting input string"
            -- dummyState


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


