module Solution exposing (..)

{-| 
-}


import Input


solveA : String -> String
solveA input =
    "implement partA here"


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


