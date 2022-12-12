module Solutions.Day12.Solution exposing (..)

{-| 
-}


import Solutions.Day12.Input as Input
import List.Extra
import Problem
import Problem.Search
import Dict

type alias Node =
    { x : Int
    , y : Int
    , height : Char
    , visited : Bool }

type alias AllNodes = List (List Node)

nodeToString : Node -> String
nodeToString { x, y, height } =
    "position: (" ++ (String.fromInt x) ++ ", " ++ (String.fromInt y) ++ ") | height: " ++ (String.fromChar height)


getAt : Int -> Int -> AllNodes -> Maybe Node
getAt x y nodes =
    List.Extra.getAt y nodes
        |> Maybe.andThen (List.Extra.getAt x)


findInitialState : AllNodes -> Maybe Node
findInitialState nodes =
    List.Extra.find (\{ height } -> height == 'S') (List.concat nodes)


isValidMove : Char -> Node -> Bool
isValidMove height node =
    if height == 'S' then
        isValidMove 'a' node
    else if node.height == 'E' then
        isValidMove height { node | height = 'z' }
    else
        (((Char.toCode node.height) - (Char.toCode height)) <= 1) && (not node.visited)


getValidMoves : AllNodes -> Node -> List Node
getValidMoves nodes node =
    List.filterMap
        (\(x, y) -> getAt x y nodes)
        [ (node.x - 1, node.y)
        , (node.x + 1, node.y)
        , (node.x, node.y - 1)
        , (node.x, node.y + 1)
        ]
        |> List.filter (isValidMove node.height)
        


initialiseProblem : AllNodes -> Maybe (Problem.Problem Node)
initialiseProblem nodes =
    case findInitialState nodes of
        Just initial ->
            Just (initialiseProblemWithNode nodes initial)
        Nothing ->
            Nothing

initialiseProblemWithNode : AllNodes -> Node -> Problem.Problem Node
initialiseProblemWithNode nodes initial =
    { initialState = initial
    , actions = \node -> getValidMoves nodes node
                    |> List.map (\valid -> { stepCost = 1.0, result = { valid | visited = True } })
    , goalTest = \{ height } -> height == 'E'
    , heuristic = \_ -> 0
    , stateToString = nodeToString
    }

parseLine : Int -> String -> List Node
parseLine y line =
    String.toList line
        |> List.indexedMap (\x height -> { x = x, y = y, height = height, visited = False })

parseInput : String -> AllNodes
parseInput input =
    String.lines input
        |> List.indexedMap parseLine


solveA : String -> String
solveA input =
    case initialiseProblem (parseInput input) of
        Just problem ->
            let
               (maybeNode, _) =
                    Problem.Search.uniformCost problem
                        |> Problem.Search.solve
            in
            case maybeNode of
                Just node ->
                    String.fromFloat node.pathCost
                Nothing ->
                    "could not find result"
                        

        Nothing ->
            "there was an issue initialising the problem"


finalValidStartingNodes : AllNodes -> List Node
finalValidStartingNodes nodes =
    List.concat nodes
        |> List.filter (\{ height } -> height == 'a' || height == 'S')


solveB : String -> String
solveB input =
    let
        nodes =
            parseInput input

        problems =
            finalValidStartingNodes nodes
                |> List.map (initialiseProblemWithNode nodes)

        
    in
    
    List.foldl 
        (\problem shortest -> 
            let
               (maybeNode, _) =
                    Problem.Search.uniformCost problem
                        |> Problem.Search.solve
            in
            case maybeNode of
                Just node ->
                    if node.pathCost < shortest then
                        node.pathCost
                    else
                        shortest
                Nothing ->
                    shortest
        )
        534 -- using my answer to part a as an initial lower bound
        problems
            |> String.fromFloat
    


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


