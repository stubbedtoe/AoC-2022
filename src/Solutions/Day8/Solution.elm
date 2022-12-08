module Solutions.Day8.Solution exposing (..)

{-| 
-}


import Solutions.Day8.Input as Input
import List.Extra
import Html.Attributes exposing (height)


type alias Tree = 
    { height : Int
    , x : Int
    , y : Int
    }

numberOfTreesVisible : List Tree -> Int -> Int
numberOfTreesVisible trees height =
    List.Extra.stoppableFoldl
        (\tree acc ->
            if tree.height >= height then
                List.Extra.Stop (acc + 1)
            else
                List.Extra.Continue (acc + 1)
        )
        0
        trees


calculateScenicScore : List Tree -> Tree -> Int
calculateScenicScore forest { x, y, height } =
    let
        neighbours =
            List.filter (\tree -> tree.x == x || tree.y == y) forest

        lookingSouth =
            List.filter (\tree -> tree.y > y) neighbours
                |> List.sortBy .y
                
        lookingNorth =
            List.filter (\tree -> tree.y < y) neighbours
                |> List.sortBy .y
                |> List.reverse

        lookingEast =
            List.filter (\tree -> tree.x > x) neighbours
                |> List.sortBy .x
        
        lookingWest =
            List.filter (\tree -> tree.x < x) neighbours
                |> List.sortBy .x
                |> List.reverse

        scoreLookingSouth =
            numberOfTreesVisible lookingSouth height

        scoreLookingNorth =
            numberOfTreesVisible lookingNorth height

        scoreLookingEast =
            numberOfTreesVisible lookingEast height

        scoreLookingWest =
            numberOfTreesVisible lookingWest height

    in
    List.product [scoreLookingEast, scoreLookingSouth, scoreLookingWest, scoreLookingNorth]


treeIsVisible : List Tree -> Int -> Int -> Tree -> Bool
treeIsVisible forest width depth { x, y, height } =
    if x == 0 || y == 0 || x == (width - 1) || y == (depth - 1) then
        True
    else
        let
            neighbours =
                List.filter (\tree -> tree.x == x || tree.y == y) forest

            lookingFromSouth =
                List.filter (\tree -> tree.y < y) neighbours

            lookingFromNorth =
                List.filter (\tree -> tree.y > y) neighbours

            lookingFromEast =
                List.filter (\tree -> tree.x < x) neighbours
            
            lookingFromWest =
                List.filter (\tree -> tree.x > x) neighbours

            canBeSeen =
                List.all (\tree -> tree.height < height) 

        in
        List.any canBeSeen [ lookingFromEast, lookingFromNorth, lookingFromSouth, lookingFromWest ]

parseLine : Int -> String -> List Tree
parseLine y line =
    String.split "" line
        |> List.filterMap String.toInt
        |> List.indexedMap (\x height -> { x = x, height = height, y = y })


solveA : String -> String
solveA input =
    let
        lines = 
            String.lines input
        forestDepth =
            List.length lines
        forestWidth =
            case List.head lines of
                Just line ->
                    String.length line
                Nothing ->
                    0
        trees = 
            List.indexedMap parseLine lines
                |> List.concatMap identity
    in
    List.filter (treeIsVisible trees forestWidth forestDepth ) trees
        |> List.length
        |> String.fromInt


solveB : String -> String
solveB input =
    let
        lines = 
            String.lines input
        trees = 
            List.indexedMap parseLine lines
                |> List.concatMap identity
    in
    List.map (calculateScenicScore trees) trees
        |> List.maximum
        |> Maybe.withDefault 0
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
            solveB Input.testInput

        "input" ->
            solveB Input.input

        _ ->
            "supported args are \"test\" or \"input\""


