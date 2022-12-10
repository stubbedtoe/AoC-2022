module Solutions.Day9.Solution exposing (..)

{-| 
-}


import Solutions.Day9.Input as Input
import List.Extra

type Move = None
    | Up 
    | Down 
    | Left 
    | Right
    | UpRight
    | UpLeft
    | DownRight
    | DownLeft

type alias Instruction =
    { direction : Move
    , steps : Int }

type alias Position =
    { y : Int
    , x : Int }


type alias State =
    { knots : List Position
    , visited : List Position }


countUniqueVisited : State -> Int
countUniqueVisited { visited } =
    List.Extra.unique visited
        |> List.length


transformPosition : Position -> Move -> Position
transformPosition { x, y } move =
    case move of
        Right ->
            { x = x + 1, y = y }
        Left ->
            { x = x - 1, y = y }
        Down ->
            { x = x, y = y - 1 }
        Up ->
            { x = x, y = y + 1 }
        UpRight ->
            { x = x + 1, y = y + 1 }
        UpLeft ->
            { x = x - 1, y = y + 1 }
        DownRight ->
            { x = x + 1, y = y - 1 }
        DownLeft ->
            { x = x - 1, y = y - 1 }
        None ->
            { x = x, y = y }


determineMove : Position -> Position -> Move
determineMove head tail =
    let 
        deltaX =
            head.x - tail.x
        deltaY =
            head.y - tail.y

        moveVertically =
            if deltaY < 0 then
                Down
            else
                Up
        moveHorizontally =
            if deltaX < 0 then
                Left
            else
                Right
        moveDiagonally =
            if deltaX > 0 && deltaY > 0 then
                UpRight
            else if deltaX < 0 && deltaY < 0 then
                DownLeft
            else if deltaX > 0 then
                DownRight
            else
                UpLeft
    in
    case (abs deltaX, abs deltaY) of
        -- moving clockwise from 12 o'clock
        (0, 2) ->
            moveVertically
        (1, 2) ->
            moveDiagonally
        (2, 2) ->
            moveDiagonally
        (2, 1) ->
            moveDiagonally
        (2, 0) ->
            moveHorizontally
        _ ->
            None


{-- mapAccuml : (a -> b -> ( a, c )) -> a -> List b -> ( a, List c )
The mapAccuml function behaves like a combination of map and foldl; 
it applies a function to each element of a list, passing an accumulating parameter from left to right, 
and returning a final value of this accumulator together with the new list.--}
executeInstruction2 : Move -> State -> State
executeInstruction2 movement state =
    let 
        (_, newPositions) =
            List.Extra.mapAccuml (\(direction, tailIndex) position ->
                let
                    newPosition =
                        transformPosition position direction
                in

                case List.Extra.getAt tailIndex state.knots of
                    Just tail ->
                        ((determineMove newPosition tail, tailIndex + 1), newPosition)
                    Nothing ->
                        ((None, tailIndex), newPosition)
            )
            (movement, 1)
            state.knots
    in
    case List.Extra.last newPositions of
        Just visited ->
            { knots = newPositions, visited = visited :: state.visited }
        Nothing ->
            state


parseLine : String -> Maybe Instruction
parseLine line =
    case String.split " " line of
        [direction, moves] ->
            let
                parsedDirection =
                    case direction of
                        "U" -> Just Up
                        "R" -> Just Right
                        "D" -> Just Down
                        "L" -> Just Left
                        _ -> Nothing
                parsedSteps =
                    String.toInt moves
            in
            case (parsedDirection, parsedSteps) of
                (Just d, Just n) ->
                    Just { direction = d
                    , steps = n }
                _ ->
                    Nothing
        _ ->
            Nothing

origin : Position
origin =
    { x = 0, y = 0 }

solveA : String -> String
solveA input =
    String.lines input
        |> List.filterMap parseLine
        |> List.map (\{ direction, steps } -> List.repeat steps direction)
        |> List.concatMap identity
        |> List.foldl
            executeInstruction2
            (initialise 1)
        |> countUniqueVisited
        |> String.fromInt



initialise : Int -> State
initialise numTailKnots =
    { knots = List.repeat (numTailKnots + 1) { x = 0, y = 0 }
    , visited = [ { x = 0, y = 0 } ] }



solveB : String -> String
solveB input =
    String.lines input
        |> List.filterMap parseLine
        |> List.map (\{ direction, steps } -> List.repeat steps direction)
        |> List.concatMap identity
        |> List.foldl
            executeInstruction2
            (initialise 9)
        |> countUniqueVisited
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
            solveB Input.testInputPt2

        "input" ->
            solveB Input.input

        _ ->
            "supported args are \"test\" or \"input\""


