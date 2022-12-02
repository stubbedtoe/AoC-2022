module Gen.Test.Runner exposing (annotation_, call_, caseOf_, formatLabels, fromTest, fuzz, getDistributionReport, getFailureReason, isTodo, make_, moduleName_, simplify, values_)

{-| 
@docs values_, call_, caseOf_, make_, annotation_, simplify, fuzz, formatLabels, getDistributionReport, isTodo, getFailureReason, fromTest, moduleName_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Test", "Runner" ]


{-| Convert a `Test` into `SeededRunners`.

In order to run any fuzz tests that the `Test` may have, it requires a default run count as well
as an initial `Random.Seed`. `100` is a good run count. To obtain a good random seed, pass a
random 32-bit integer to `Random.initialSeed`. You can obtain such an integer by running
`Math.floor(Math.random()*0xFFFFFFFF)` in Node. It's typically fine to hard-code this value into
your Elm code; it's easy and makes your tests reproducible.

fromTest: Int -> Random.Seed -> Test.Test -> Test.Runner.SeededRunners
-}
fromTest : Int -> Elm.Expression -> Elm.Expression -> Elm.Expression
fromTest fromTestArg fromTestArg0 fromTestArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Runner" ]
            , name = "fromTest"
            , annotation =
                Just
                    (Type.function
                        [ Type.int
                        , Type.namedWith [ "Random" ] "Seed" []
                        , Type.namedWith [ "Test" ] "Test" []
                        ]
                        (Type.namedWith [ "Test", "Runner" ] "SeededRunners" [])
                    )
            }
        )
        [ Elm.int fromTestArg, fromTestArg0, fromTestArg1 ]


{-| Return `Nothing` if the given [`Expectation`](Expect#Expectation) is a [`pass`](Expect#pass).

If it is a [`fail`](Expect#fail), return a record containing the expectation
description, the [`Reason`](Test-Runner-Failure#Reason) the test failed, and the given inputs if
it was a fuzz test. (If it was not a fuzz test, the record's `given` field
will be `Nothing`).

For example:

    getFailureReason (Expect.equal 1 2)
    -- Just { reason = Equal 1 2, description = "Expect.equal", given = Nothing }

    getFailureReason (Expect.equal 1 1)
    -- Nothing

getFailureReason: 
    Expect.Expectation
    -> Maybe { given : Maybe String
    , description : String
    , reason : Test.Runner.Failure.Reason
    }
-}
getFailureReason : Elm.Expression -> Elm.Expression
getFailureReason getFailureReasonArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Runner" ]
            , name = "getFailureReason"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Expect" ] "Expectation" [] ]
                        (Type.maybe
                            (Type.record
                                [ ( "given", Type.maybe Type.string )
                                , ( "description", Type.string )
                                , ( "reason"
                                  , Type.namedWith
                                        [ "Test", "Runner", "Failure" ]
                                        "Reason"
                                        []
                                  )
                                ]
                            )
                        )
                    )
            }
        )
        [ getFailureReasonArg ]


{-| Determine if an expectation was created by a call to `Test.todo`. Runners
may treat these tests differently in their output.

isTodo: Expect.Expectation -> Bool
-}
isTodo : Elm.Expression -> Elm.Expression
isTodo isTodoArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Runner" ]
            , name = "isTodo"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Expect" ] "Expectation" [] ]
                        Type.bool
                    )
            }
        )
        [ isTodoArg ]


{-| Returns a `DistributionReport` computed for a given test.

getDistributionReport: Expect.Expectation -> Test.Distribution.DistributionReport
-}
getDistributionReport : Elm.Expression -> Elm.Expression
getDistributionReport getDistributionReportArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Runner" ]
            , name = "getDistributionReport"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Expect" ] "Expectation" [] ]
                        (Type.namedWith
                            [ "Test", "Distribution" ]
                            "DistributionReport"
                            []
                        )
                    )
            }
        )
        [ getDistributionReportArg ]


{-| A standard way to format descriptions and test labels, to keep things
consistent across test runner implementations.

The HTML, Node, String, and Log runners all use this.

What it does:

  - drop any labels that are empty strings
  - format the first label differently from the others
  - reverse the resulting list

Example:

    [ "the actual test that failed"
    , "nested description failure"
    , "top-level description failure"
    ]
    |> formatLabels ((++) "↓ ") ((++) "✗ ")

    {-
    [ "↓ top-level description failure"
    , "↓ nested description failure"
    , "✗ the actual test that failed"
    ]
    -}

formatLabels: (String -> format) -> (String -> format) -> List String -> List format
-}
formatLabels :
    (Elm.Expression -> Elm.Expression)
    -> (Elm.Expression -> Elm.Expression)
    -> List String
    -> Elm.Expression
formatLabels formatLabelsArg formatLabelsArg0 formatLabelsArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Runner" ]
            , name = "formatLabels"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "format")
                        , Type.function [ Type.string ] (Type.var "format")
                        , Type.list Type.string
                        ]
                        (Type.list (Type.var "format"))
                    )
            }
        )
        [ Elm.functionReduced "formatLabelsUnpack" formatLabelsArg
        , Elm.functionReduced "formatLabelsUnpack" formatLabelsArg0
        , Elm.list (List.map Elm.string formatLabelsArg1)
        ]


{-| Given a fuzzer, return a random generator to produce a value and a
Simplifiable. The value is what a fuzz test would have received as input.

Note that fuzzers aren't generated to succeed, which is why this function returns
a Result. The String inside the Err case will contain a failure reason.

fuzz: 
    Fuzz.Fuzzer a
    -> Random.Generator (Result.Result String ( a, Test.Runner.Simplifiable a ))
-}
fuzz : Elm.Expression -> Elm.Expression
fuzz fuzzArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Runner" ]
            , name = "fuzz"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ] ]
                        (Type.namedWith
                            [ "Random" ]
                            "Generator"
                            [ Type.namedWith
                                [ "Result" ]
                                "Result"
                                [ Type.string
                                , Type.tuple
                                    (Type.var "a")
                                    (Type.namedWith
                                        [ "Test", "Runner" ]
                                        "Simplifiable"
                                        [ Type.var "a" ]
                                    )
                                ]
                            ]
                        )
                    )
            }
        )
        [ fuzzArg ]


{-| Given a Simplifiable, simplify the value further. Pass your test function to
drive the simplification process: if a simplified value passes the test, it will
be discarded. In this sense, you will get the simplest value that still fails
your test.

simplify: 
    (a -> Expect.Expectation)
    -> ( a, Test.Runner.Simplifiable a )
    -> Maybe ( a, Test.Runner.Simplifiable a )
-}
simplify :
    (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
simplify simplifyArg simplifyArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Runner" ]
            , name = "simplify"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a" ]
                            (Type.namedWith [ "Expect" ] "Expectation" [])
                        , Type.tuple
                            (Type.var "a")
                            (Type.namedWith
                                [ "Test", "Runner" ]
                                "Simplifiable"
                                [ Type.var "a" ]
                            )
                        ]
                        (Type.maybe
                            (Type.tuple
                                (Type.var "a")
                                (Type.namedWith
                                    [ "Test", "Runner" ]
                                    "Simplifiable"
                                    [ Type.var "a" ]
                                )
                            )
                        )
                    )
            }
        )
        [ Elm.functionReduced "simplifyUnpack" simplifyArg, simplifyArg0 ]


annotation_ :
    { runner : Type.Annotation
    , seededRunners : Type.Annotation
    , simplifiable : Type.Annotation -> Type.Annotation
    }
annotation_ =
    { runner =
        Type.alias
            moduleName_
            "Runner"
            []
            (Type.record
                [ ( "run"
                  , Type.function
                        [ Type.unit ]
                        (Type.list
                            (Type.namedWith [ "Expect" ] "Expectation" [])
                        )
                  )
                , ( "labels", Type.list Type.string )
                ]
            )
    , seededRunners = Type.namedWith [ "Test", "Runner" ] "SeededRunners" []
    , simplifiable =
        \simplifiableArg0 ->
            Type.namedWith
                [ "Test", "Runner" ]
                "Simplifiable"
                [ simplifiableArg0 ]
    }


make_ :
    { runner :
        { run : Elm.Expression, labels : Elm.Expression } -> Elm.Expression
    , plain : Elm.Expression -> Elm.Expression
    , only : Elm.Expression -> Elm.Expression
    , skipping : Elm.Expression -> Elm.Expression
    , invalid : Elm.Expression -> Elm.Expression
    }
make_ =
    { runner =
        \runner_args ->
            Elm.withType
                (Type.alias
                    [ "Test", "Runner" ]
                    "Runner"
                    []
                    (Type.record
                        [ ( "run"
                          , Type.function
                                [ Type.unit ]
                                (Type.list
                                    (Type.namedWith
                                        [ "Expect" ]
                                        "Expectation"
                                        []
                                    )
                                )
                          )
                        , ( "labels", Type.list Type.string )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "run" runner_args.run
                    , Tuple.pair "labels" runner_args.labels
                    ]
                )
    , plain =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Runner" ]
                    , name = "Plain"
                    , annotation = Just (Type.namedWith [] "SeededRunners" [])
                    }
                )
                [ ar0 ]
    , only =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Runner" ]
                    , name = "Only"
                    , annotation = Just (Type.namedWith [] "SeededRunners" [])
                    }
                )
                [ ar0 ]
    , skipping =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Runner" ]
                    , name = "Skipping"
                    , annotation = Just (Type.namedWith [] "SeededRunners" [])
                    }
                )
                [ ar0 ]
    , invalid =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Runner" ]
                    , name = "Invalid"
                    , annotation = Just (Type.namedWith [] "SeededRunners" [])
                    }
                )
                [ ar0 ]
    }


caseOf_ :
    { seededRunners :
        Elm.Expression
        -> { seededRunnersTags_0_0
            | plain : Elm.Expression -> Elm.Expression
            , only : Elm.Expression -> Elm.Expression
            , skipping : Elm.Expression -> Elm.Expression
            , invalid : Elm.Expression -> Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { seededRunners =
        \seededRunnersExpression seededRunnersTags ->
            Elm.Case.custom
                seededRunnersExpression
                (Type.namedWith [ "Test", "Runner" ] "SeededRunners" [])
                [ Elm.Case.branch1
                    "Plain"
                    ( "list.List"
                    , Type.list
                        (Type.namedWith [ "Test", "Runner" ] "Runner" [])
                    )
                    seededRunnersTags.plain
                , Elm.Case.branch1
                    "Only"
                    ( "list.List"
                    , Type.list
                        (Type.namedWith [ "Test", "Runner" ] "Runner" [])
                    )
                    seededRunnersTags.only
                , Elm.Case.branch1
                    "Skipping"
                    ( "list.List"
                    , Type.list
                        (Type.namedWith [ "Test", "Runner" ] "Runner" [])
                    )
                    seededRunnersTags.skipping
                , Elm.Case.branch1
                    "Invalid"
                    ( "string.String", Type.string )
                    seededRunnersTags.invalid
                ]
    }


call_ :
    { fromTest :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , getFailureReason : Elm.Expression -> Elm.Expression
    , isTodo : Elm.Expression -> Elm.Expression
    , getDistributionReport : Elm.Expression -> Elm.Expression
    , formatLabels :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , fuzz : Elm.Expression -> Elm.Expression
    , simplify : Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { fromTest =
        \fromTestArg fromTestArg0 fromTestArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Runner" ]
                    , name = "fromTest"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int
                                , Type.namedWith [ "Random" ] "Seed" []
                                , Type.namedWith [ "Test" ] "Test" []
                                ]
                                (Type.namedWith
                                    [ "Test", "Runner" ]
                                    "SeededRunners"
                                    []
                                )
                            )
                    }
                )
                [ fromTestArg, fromTestArg0, fromTestArg1 ]
    , getFailureReason =
        \getFailureReasonArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Runner" ]
                    , name = "getFailureReason"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Expect" ] "Expectation" [] ]
                                (Type.maybe
                                    (Type.record
                                        [ ( "given", Type.maybe Type.string )
                                        , ( "description", Type.string )
                                        , ( "reason"
                                          , Type.namedWith
                                                [ "Test", "Runner", "Failure" ]
                                                "Reason"
                                                []
                                          )
                                        ]
                                    )
                                )
                            )
                    }
                )
                [ getFailureReasonArg ]
    , isTodo =
        \isTodoArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Runner" ]
                    , name = "isTodo"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Expect" ] "Expectation" [] ]
                                Type.bool
                            )
                    }
                )
                [ isTodoArg ]
    , getDistributionReport =
        \getDistributionReportArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Runner" ]
                    , name = "getDistributionReport"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Expect" ] "Expectation" [] ]
                                (Type.namedWith
                                    [ "Test", "Distribution" ]
                                    "DistributionReport"
                                    []
                                )
                            )
                    }
                )
                [ getDistributionReportArg ]
    , formatLabels =
        \formatLabelsArg formatLabelsArg0 formatLabelsArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Runner" ]
                    , name = "formatLabels"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.string ]
                                    (Type.var "format")
                                , Type.function
                                    [ Type.string ]
                                    (Type.var "format")
                                , Type.list Type.string
                                ]
                                (Type.list (Type.var "format"))
                            )
                    }
                )
                [ formatLabelsArg, formatLabelsArg0, formatLabelsArg1 ]
    , fuzz =
        \fuzzArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Runner" ]
                    , name = "fuzz"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Random" ]
                                    "Generator"
                                    [ Type.namedWith
                                        [ "Result" ]
                                        "Result"
                                        [ Type.string
                                        , Type.tuple
                                            (Type.var "a")
                                            (Type.namedWith
                                                [ "Test", "Runner" ]
                                                "Simplifiable"
                                                [ Type.var "a" ]
                                            )
                                        ]
                                    ]
                                )
                            )
                    }
                )
                [ fuzzArg ]
    , simplify =
        \simplifyArg simplifyArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Runner" ]
                    , name = "simplify"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a" ]
                                    (Type.namedWith
                                        [ "Expect" ]
                                        "Expectation"
                                        []
                                    )
                                , Type.tuple
                                    (Type.var "a")
                                    (Type.namedWith
                                        [ "Test", "Runner" ]
                                        "Simplifiable"
                                        [ Type.var "a" ]
                                    )
                                ]
                                (Type.maybe
                                    (Type.tuple
                                        (Type.var "a")
                                        (Type.namedWith
                                            [ "Test", "Runner" ]
                                            "Simplifiable"
                                            [ Type.var "a" ]
                                        )
                                    )
                                )
                            )
                    }
                )
                [ simplifyArg, simplifyArg0 ]
    }


values_ :
    { fromTest : Elm.Expression
    , getFailureReason : Elm.Expression
    , isTodo : Elm.Expression
    , getDistributionReport : Elm.Expression
    , formatLabels : Elm.Expression
    , fuzz : Elm.Expression
    , simplify : Elm.Expression
    }
values_ =
    { fromTest =
        Elm.value
            { importFrom = [ "Test", "Runner" ]
            , name = "fromTest"
            , annotation =
                Just
                    (Type.function
                        [ Type.int
                        , Type.namedWith [ "Random" ] "Seed" []
                        , Type.namedWith [ "Test" ] "Test" []
                        ]
                        (Type.namedWith [ "Test", "Runner" ] "SeededRunners" [])
                    )
            }
    , getFailureReason =
        Elm.value
            { importFrom = [ "Test", "Runner" ]
            , name = "getFailureReason"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Expect" ] "Expectation" [] ]
                        (Type.maybe
                            (Type.record
                                [ ( "given", Type.maybe Type.string )
                                , ( "description", Type.string )
                                , ( "reason"
                                  , Type.namedWith
                                        [ "Test", "Runner", "Failure" ]
                                        "Reason"
                                        []
                                  )
                                ]
                            )
                        )
                    )
            }
    , isTodo =
        Elm.value
            { importFrom = [ "Test", "Runner" ]
            , name = "isTodo"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Expect" ] "Expectation" [] ]
                        Type.bool
                    )
            }
    , getDistributionReport =
        Elm.value
            { importFrom = [ "Test", "Runner" ]
            , name = "getDistributionReport"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Expect" ] "Expectation" [] ]
                        (Type.namedWith
                            [ "Test", "Distribution" ]
                            "DistributionReport"
                            []
                        )
                    )
            }
    , formatLabels =
        Elm.value
            { importFrom = [ "Test", "Runner" ]
            , name = "formatLabels"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.string ] (Type.var "format")
                        , Type.function [ Type.string ] (Type.var "format")
                        , Type.list Type.string
                        ]
                        (Type.list (Type.var "format"))
                    )
            }
    , fuzz =
        Elm.value
            { importFrom = [ "Test", "Runner" ]
            , name = "fuzz"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ] ]
                        (Type.namedWith
                            [ "Random" ]
                            "Generator"
                            [ Type.namedWith
                                [ "Result" ]
                                "Result"
                                [ Type.string
                                , Type.tuple
                                    (Type.var "a")
                                    (Type.namedWith
                                        [ "Test", "Runner" ]
                                        "Simplifiable"
                                        [ Type.var "a" ]
                                    )
                                ]
                            ]
                        )
                    )
            }
    , simplify =
        Elm.value
            { importFrom = [ "Test", "Runner" ]
            , name = "simplify"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a" ]
                            (Type.namedWith [ "Expect" ] "Expectation" [])
                        , Type.tuple
                            (Type.var "a")
                            (Type.namedWith
                                [ "Test", "Runner" ]
                                "Simplifiable"
                                [ Type.var "a" ]
                            )
                        ]
                        (Type.maybe
                            (Type.tuple
                                (Type.var "a")
                                (Type.namedWith
                                    [ "Test", "Runner" ]
                                    "Simplifiable"
                                    [ Type.var "a" ]
                                )
                            )
                        )
                    )
            }
    }


