module Gen.Input exposing (expectedA, expectedB, input, moduleName_, testInput, values_)

{-| 
@docs values_, testInput, expectedA, expectedB, input, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Input" ]


{-| input: String -}
input : Elm.Expression
input =
    Elm.value
        { importFrom = [ "Input" ]
        , name = "input"
        , annotation = Just Type.string
        }


{-| expectedB: String -}
expectedB : Elm.Expression
expectedB =
    Elm.value
        { importFrom = [ "Input" ]
        , name = "expectedB"
        , annotation = Just Type.string
        }


{-| expectedA: String -}
expectedA : Elm.Expression
expectedA =
    Elm.value
        { importFrom = [ "Input" ]
        , name = "expectedA"
        , annotation = Just Type.string
        }


{-| testInput: String -}
testInput : Elm.Expression
testInput =
    Elm.value
        { importFrom = [ "Input" ]
        , name = "testInput"
        , annotation = Just Type.string
        }


values_ :
    { input : Elm.Expression
    , expectedB : Elm.Expression
    , expectedA : Elm.Expression
    , testInput : Elm.Expression
    }
values_ =
    { input =
        Elm.value
            { importFrom = [ "Input" ]
            , name = "input"
            , annotation = Just Type.string
            }
    , expectedB =
        Elm.value
            { importFrom = [ "Input" ]
            , name = "expectedB"
            , annotation = Just Type.string
            }
    , expectedA =
        Elm.value
            { importFrom = [ "Input" ]
            , name = "expectedA"
            , annotation = Just Type.string
            }
    , testInput =
        Elm.value
            { importFrom = [ "Input" ]
            , name = "testInput"
            , annotation = Just Type.string
            }
    }


