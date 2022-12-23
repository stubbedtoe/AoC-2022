module Solutions.Day16.Solution exposing (..)

{-| 
-}


import Solutions.Day16.Input as Input
import Parser exposing (Parser, Trailing(..), sequence, oneOf, spaces, variable, int, succeed, symbol, keyword, (|.), (|=))
import Set
import Problem
import Problem.Search
import Dict

type alias AllValves = Dict.Dict String Valve

type alias Valve =
    { flowRate : Int
    , tunnels : List String
    , open : Bool
    }


type alias State =
    { current : String
    , sumOfOpenValves : Int
    , timeLeft : Int
    , allValves : AllValves
    }


stateToString : State -> String
stateToString { current, sumOfOpenValves, timeLeft } =
    "current: "
        ++ current
        ++ "; sum of open: "
        ++ (String.fromInt (negate sumOfOpenValves))
        ++ "; time left: "
        ++ (String.fromInt timeLeft)
        ++ " seconds."


initialiseState : AllValves -> State
initialiseState allValves =
    { current = "AA"
    , sumOfOpenValves = 0
    , timeLeft = 30
    , allValves = allValves
    }


moveToValve : State -> String -> State
moveToValve state label =
    case Dict.get label state.allValves of
        Just valve ->
            if (valve.flowRate == 0 || valve.open) then
                { state | current = label
                , timeLeft = state.timeLeft - 1
                }
            else
                { current = label
                , timeLeft = state.timeLeft - 2
                , sumOfOpenValves = state.sumOfOpenValves + (valve.flowRate * (state.timeLeft - 1))
                , allValves = Dict.insert label { valve | open = True } state.allValves
                }
        Nothing ->
            state


initialiseProblem : AllValves -> Problem.Problem State
initialiseProblem allValves =
    { initialState = initialiseState allValves
    , actions = \state -> getValidMoves state
                            |> List.map (moveToValve state)
                            |> List.map (\newState -> { stepCost = (toFloat newState.sumOfOpenValves), result = newState })
    , goalTest = \state -> state.timeLeft == 0
    , heuristic = \_ -> 0
    , stateToString = \_ -> ""
    }

    


getValidMoves : State -> List String
getValidMoves { current, allValves } =
    case Dict.get current allValves of
        Just valve ->
            valve.tunnels
        Nothing ->
            []


parseLabel : Parser String
parseLabel =
    variable
    { start = Char.isUpper
    , inner = Char.isUpper
    , reserved = Set.empty
    }


parseInput : String -> AllValves
parseInput =
    String.lines
        >> List.map (Parser.run parseLine) 
        >> List.filterMap Result.toMaybe 
        >> Dict.fromList



parseLine : Parser (String, Valve)
parseLine =
    succeed (\label flowRate tunnels -> (label, { flowRate = negate flowRate, tunnels = tunnels, open = False }))
        |. keyword "Valve"
        |. spaces
        |= parseLabel
        |. spaces
        |. keyword "has flow rate"
        |. symbol "="
        |= int
        |. symbol ";"
        |. spaces
        |. oneOf
            [ keyword "tunnels lead to valves"
            , keyword "tunnel leads to valve"  
            ]
        |. spaces
        |= sequence { start = ""
            , separator = ","
            , end = ""
            , spaces = spaces
            , item = parseLabel
            , trailing = Forbidden
            }

solveA : String -> String
solveA input =
    let
        initialProblem =
            initialiseProblem (parseInput input)

        (maybeState, _) =
            Problem.Search.uniformCost initialProblem
                |> Problem.Search.solve
    in
    case maybeState of
        Just state ->
            String.fromInt state.state.sumOfOpenValves
        Nothing ->
            "could not find result"


solveB : String -> String
solveB input =
    "implement partB here"


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


