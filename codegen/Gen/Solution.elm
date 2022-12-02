module Gen.Solution exposing (call_, moduleName_, partA, partB, solveA, solveB, values_)

{-| 
@docs values_, call_, solveA, solveB, partA, partB, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Solution" ]


{-| partB: String -> String -}
partB : String -> Elm.Expression
partB partBArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Solution" ]
            , name = "partB"
            , annotation = Just (Type.function [ Type.string ] Type.string)
            }
        )
        [ Elm.string partBArg ]


{-| partA: String -> String -}
partA : String -> Elm.Expression
partA partAArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Solution" ]
            , name = "partA"
            , annotation = Just (Type.function [ Type.string ] Type.string)
            }
        )
        [ Elm.string partAArg ]


{-| solveB: String -> String -}
solveB : String -> Elm.Expression
solveB solveBArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Solution" ]
            , name = "solveB"
            , annotation = Just (Type.function [ Type.string ] Type.string)
            }
        )
        [ Elm.string solveBArg ]


{-| solveA: String -> String -}
solveA : String -> Elm.Expression
solveA solveAArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Solution" ]
            , name = "solveA"
            , annotation = Just (Type.function [ Type.string ] Type.string)
            }
        )
        [ Elm.string solveAArg ]


call_ :
    { partB : Elm.Expression -> Elm.Expression
    , partA : Elm.Expression -> Elm.Expression
    , solveB : Elm.Expression -> Elm.Expression
    , solveA : Elm.Expression -> Elm.Expression
    }
call_ =
    { partB =
        \partBArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Solution" ]
                    , name = "partB"
                    , annotation =
                        Just (Type.function [ Type.string ] Type.string)
                    }
                )
                [ partBArg ]
    , partA =
        \partAArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Solution" ]
                    , name = "partA"
                    , annotation =
                        Just (Type.function [ Type.string ] Type.string)
                    }
                )
                [ partAArg ]
    , solveB =
        \solveBArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Solution" ]
                    , name = "solveB"
                    , annotation =
                        Just (Type.function [ Type.string ] Type.string)
                    }
                )
                [ solveBArg ]
    , solveA =
        \solveAArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Solution" ]
                    , name = "solveA"
                    , annotation =
                        Just (Type.function [ Type.string ] Type.string)
                    }
                )
                [ solveAArg ]
    }


values_ :
    { partB : Elm.Expression
    , partA : Elm.Expression
    , solveB : Elm.Expression
    , solveA : Elm.Expression
    }
values_ =
    { partB =
        Elm.value
            { importFrom = [ "Solution" ]
            , name = "partB"
            , annotation = Just (Type.function [ Type.string ] Type.string)
            }
    , partA =
        Elm.value
            { importFrom = [ "Solution" ]
            , name = "partA"
            , annotation = Just (Type.function [ Type.string ] Type.string)
            }
    , solveB =
        Elm.value
            { importFrom = [ "Solution" ]
            , name = "solveB"
            , annotation = Just (Type.function [ Type.string ] Type.string)
            }
    , solveA =
        Elm.value
            { importFrom = [ "Solution" ]
            , name = "solveA"
            , annotation = Just (Type.function [ Type.string ] Type.string)
            }
    }


