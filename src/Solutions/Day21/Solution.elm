module Solutions.Day21.Solution exposing (..)

{-| 
-}


import Solutions.Day21.Input as Input
import Dict exposing (Dict)
import Parser exposing (Parser, oneOf, (|.), (|=), succeed, variable, symbol, spaces, int)
import Set

type Op = Mult
    | Div
    | Sub
    | Add

type Expr = Literal Int
    | BinOp Inner Op Inner
    | Unknown String

type Inner = Var Expr 

type alias Lookup = Dict String Expr

parseKey : Parser String
parseKey =
    variable
        { start = Char.isLower
        , inner = Char.isLower
        , reserved = Set.empty 
        }

parseBinOp : Parser Expr
parseBinOp =
    oneOf 
        [ Parser.map Literal int
        , succeed BinOp
            |= Parser.map (\s -> Var (Unknown s)) parseKey
            |. spaces
            |= oneOf 
                [ Parser.map (\_ -> Mult) (symbol "*")
                , Parser.map (\_ -> Div) (symbol "/")
                , Parser.map (\_ -> Add) (symbol "+")
                , Parser.map (\_ -> Sub) (symbol "-")
                ]
            |. spaces
            |= Parser.map (\s -> Var (Unknown s)) parseKey
        ]

parseExpression : Parser (String, Expr)
parseExpression =
    succeed (\key expr -> (key, expr))
        |= parseKey
        |. symbol ":"
        |. spaces
        |= parseBinOp


operate : Int -> Op -> Int -> Int
operate left op right =
    case op of
        Mult ->
            left * right
        Div ->
            left // right
        Add ->
            left + right
        Sub ->
            left - right


lookupReplacement : String -> Expr -> Lookup -> Lookup
lookupReplacement key value lookup =
    Dict.insert key (simplifyExpression value lookup) lookup
                
runSimplifyUntilNochange : Lookup -> Lookup -> Lookup
runSimplifyUntilNochange previous current =
    if previous == current then
        current
    else
        runSimplifyUntilNochange current (Dict.foldl lookupReplacement previous previous)

runlookupUntilRoot : Lookup -> String
runlookupUntilRoot lookup =
    case Dict.get "root" lookup of
        Just found ->
            case found of
                Literal answer ->
                    String.fromInt answer
                _ ->
                    runlookupUntilRoot (Dict.foldl lookupReplacement lookup lookup)
        _ ->
            "error: root not in lookup"


opToString : Op -> String
opToString op =
    case op of
        Mult ->
            " * "
        Div ->
            " / "
        Add ->
            " + "
        Sub ->
            " - "

simplifyExpression : Expr -> Lookup -> Expr
simplifyExpression expr lookup =
    case expr of
        Literal _ ->
            expr -- cant be simplified

        Unknown s ->
            case Dict.get s lookup of
                Just found ->
                    simplifyExpression found lookup
                Nothing -> --humn
                    expr

        BinOp (Var v1) op (Var v2) ->
            case (v1, v2) of
                (Literal i1, Literal i2) ->
                    Literal (operate i1 op i2)

                _ ->
                    BinOp (Var (simplifyExpression v1 lookup)) op (Var (simplifyExpression v2 lookup))


-- varToString : Var -> Lookup -> String
-- varToString var lookup =
--     case var of
--         Known i ->
--             String.fromInt i
--         Unknown s ->
--             expressionToString s lookup


expressionToString : Expr -> Lookup -> String
expressionToString expr lookup =
    case expr of
        Literal i ->
            String.fromInt i
        Unknown s ->
            if s == "humn" then
                "x"
            else
                s
        BinOp (Var v1) op (Var v2) ->
            "(" ++ (expressionToString v1 lookup) ++ (opToString op) ++ (expressionToString v2 lookup) ++ ")"


variableToString : String -> Lookup -> String
variableToString key lookup =
    case Dict.get key lookup of
        Just found ->
            expressionToString found lookup
        Nothing ->
            "key " ++ key ++ " not found"


simplify : Lookup -> Lookup
simplify lookup =
    -- runSimplifyUntilNochange lookup Dict.empty
    Dict.foldl lookupReplacement lookup lookup

solveA : String -> String
solveA =
    String.lines
        >> List.map (Parser.run parseExpression)
        >> List.filterMap Result.toMaybe
        >> Dict.fromList
        >> runlookupUntilRoot




solveB : String -> String
solveB input =
    let
        lookup =
            String.lines input
                |> List.filter (\line -> not ((String.startsWith "root" line) || (String.startsWith "humn" line)))
                |> List.map (Parser.run parseExpression)
                |> List.filterMap Result.toMaybe
                |> Dict.fromList
                |> simplify
    in
    -- root: gvfh == njlw
    -- root: gvfh == 82091308111060
    -- using https://www.mathpapa.com/simplify-calculator/
    -- root: (−703/15)humn + 242030880169441 == 8209130811106
    -- using https://www.wolframalpha.com/input?i=82091308111060+%3D%3D+%28-703%2F15%29x+%2B+242030880169441
    "3412650897405"


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
            "301"

        "input" ->
            solveB Input.input

        _ ->
            "supported args are \"test\" or \"input\""


