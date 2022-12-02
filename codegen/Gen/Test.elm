module Gen.Test exposing (annotation_, call_, concat, describe, expectDistribution, fuzz, fuzz2, fuzz3, fuzzWith, make_, moduleName_, noDistribution, only, reportDistribution, skip, test, todo, values_)

{-| 
@docs values_, call_, make_, annotation_, expectDistribution, reportDistribution, noDistribution, fuzzWith, fuzz3, fuzz2, fuzz, only, skip, todo, concat, describe, test, moduleName_
-}


import Elm
import Elm.Annotation as Type
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Test" ]


{-| Return a [`Test`](#Test) that evaluates a single
[`Expectation`](../Expect#Expectation).

    import Test exposing (fuzz)
    import Expect


    test "the empty list has 0 length" <|
        \_ ->
            List.length []
                |> Expect.equal 0

test: String -> (() -> Expect.Expectation) -> Test.Test
-}
test : String -> (Elm.Expression -> Elm.Expression) -> Elm.Expression
test testArg testArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test" ]
            , name = "test"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.function
                            [ Type.unit ]
                            (Type.namedWith [ "Expect" ] "Expectation" [])
                        ]
                        (Type.namedWith [ "Test" ] "Test" [])
                    )
            }
        )
        [ Elm.string testArg, Elm.functionReduced "testUnpack" testArg0 ]


{-| Apply a description to a list of tests.

    import Test exposing (describe, test, fuzz)
    import Fuzz exposing (int)
    import Expect


    describe "List"
        [ describe "reverse"
            [ test "has no effect on an empty list" <|
                \_ ->
                    List.reverse []
                        |> Expect.equal []
            , fuzz int "has no effect on a one-item list" <|
                \num ->
                     List.reverse [ num ]
                        |> Expect.equal [ num ]
            ]
        ]

Passing an empty list will result in a failing test, because you either made a
mistake or are creating a placeholder.

describe: String -> List Test.Test -> Test.Test
-}
describe : String -> List Elm.Expression -> Elm.Expression
describe describeArg describeArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test" ]
            , name = "describe"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list (Type.namedWith [ "Test" ] "Test" [])
                        ]
                        (Type.namedWith [ "Test" ] "Test" [])
                    )
            }
        )
        [ Elm.string describeArg, Elm.list describeArg0 ]


{-| Run each of the given tests.

    concat [ testDecoder, testSorting ]

concat: List Test.Test -> Test.Test
-}
concat : List Elm.Expression -> Elm.Expression
concat concatArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test" ]
            , name = "concat"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Test" ] "Test" []) ]
                        (Type.namedWith [ "Test" ] "Test" [])
                    )
            }
        )
        [ Elm.list concatArg ]


{-| Returns a [`Test`](#Test) that is "TODO" (not yet implemented). These tests
always fail, but test runners will only include them in their output if there
are no other failures.

These tests aren't meant to be committed to version control. Instead, use them
when you're brainstorming lots of tests you'd like to write, but you can't
implement them all at once. When you replace `todo` with a real test, you'll be
able to see if it fails without clutter from tests still not implemented. But,
unlike leaving yourself comments, you'll be prompted to implement these tests
because your suite will fail.

    describe "a new thing"
        [ todo "does what is expected in the common case"
        , todo "correctly handles an edge case I just thought of"
        ]

This functionality is similar to "pending" tests in other frameworks, except
that a TODO test is considered failing but a pending test often is not.

todo: String -> Test.Test
-}
todo : String -> Elm.Expression
todo todoArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test" ]
            , name = "todo"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Test" ] "Test" [])
                    )
            }
        )
        [ Elm.string todoArg ]


{-| Returns a [`Test`](#Test) that gets skipped.

Calls to `skip` aren't meant to be committed to version control. Instead, use
it when you want to focus on getting a particular subset of your tests to
pass. If you use `skip`, your entire test suite will fail, even if
each of the individual tests pass. This is to help avoid accidentally
committing a `skip` to version control.

See also [`only`](#only). Note that `skip` takes precedence over `only`;
if you use a `skip` inside an `only`, it will still get skipped, and if you use
an `only` inside a `skip`, it will also get skipped.

    describe "List"
        [ skip <|
            describe "reverse"
                [ test "has no effect on an empty list" <|
                    \_ ->
                        List.reverse []
                            |> Expect.equal []
                , fuzz int "has no effect on a one-item list" <|
                    \num ->
                        List.reverse [ num ]
                            |> Expect.equal [ num ]
                ]
        , test "This is the only test that will get run; the other was skipped!" <|
            \_ ->
                List.length []
                    |> Expect.equal 0
        ]

skip: Test.Test -> Test.Test
-}
skip : Elm.Expression -> Elm.Expression
skip skipArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test" ]
            , name = "skip"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Test" ] "Test" [] ]
                        (Type.namedWith [ "Test" ] "Test" [])
                    )
            }
        )
        [ skipArg ]


{-| Returns a [`Test`](#Test) that causes other tests to be skipped, and
only runs the given one.

Calls to `only` aren't meant to be committed to version control. Instead, use
them when you want to focus on getting a particular subset of your tests to pass.
If you use `only`, your entire test suite will fail, even if
each of the individual tests pass. This is to help avoid accidentally
committing a `only` to version control.

If you you use `only` on multiple tests, only those tests will run. If you
put a `only` inside another `only`, only the outermost `only`
will affect which tests gets run.

See also [`skip`](#skip). Note that `skip` takes precedence over `only`;
if you use a `skip` inside an `only`, it will still get skipped, and if you use
an `only` inside a `skip`, it will also get skipped.

    describe "List"
        [ only <|
            describe "reverse"
                [ test "has no effect on an empty list" <|
                    \_ ->
                        List.reverse []
                            |> Expect.equal []
                , fuzz int "has no effect on a one-item list" <|
                    \num ->
                        List.reverse [ num ]
                            |> Expect.equal [ num ]
                ]
        , test "This will not get run, because of the `only` above!" <|
            \_ ->
                List.length []
                    |> Expect.equal 0
        ]

only: Test.Test -> Test.Test
-}
only : Elm.Expression -> Elm.Expression
only onlyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test" ]
            , name = "only"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Test" ] "Test" [] ]
                        (Type.namedWith [ "Test" ] "Test" [])
                    )
            }
        )
        [ onlyArg ]


{-| Take a function that produces a test, and calls it several (usually 100) times, using a randomly-generated input
from a [`Fuzzer`](http://package.elm-lang.org/packages/elm-explorations/test/latest/Fuzz) each time. This allows you to
test that a property that should always be true is indeed true under a wide variety of conditions. The function also
takes a string describing the test.

These are called "[fuzz tests](https://en.wikipedia.org/wiki/Fuzz_testing)" because of the randomness.
You may find them elsewhere called [property-based tests](http://blog.jessitron.com/2013/04/property-based-testing-what-is-it.html),
[generative tests](http://www.pivotaltracker.com/community/tracker-blog/generative-testing), or
[QuickCheck-style tests](https://en.wikipedia.org/wiki/QuickCheck).

    import Test exposing (fuzz)
    import Fuzz exposing (list, int)
    import Expect


    fuzz (list int) "List.length should never be negative" <|
        -- This anonymous function will be run 100 times, each time with a
        -- randomly-generated fuzzList value.
        \fuzzList ->
            fuzzList
                |> List.length
                |> Expect.atLeast 0

fuzz: Fuzz.Fuzzer a -> String -> (a -> Expect.Expectation) -> Test.Test
-}
fuzz :
    Elm.Expression
    -> String
    -> (Elm.Expression -> Elm.Expression)
    -> Elm.Expression
fuzz fuzzArg fuzzArg0 fuzzArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test" ]
            , name = "fuzz"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.string
                        , Type.function
                            [ Type.var "a" ]
                            (Type.namedWith [ "Expect" ] "Expectation" [])
                        ]
                        (Type.namedWith [ "Test" ] "Test" [])
                    )
            }
        )
        [ fuzzArg
        , Elm.string fuzzArg0
        , Elm.functionReduced "fuzzUnpack" fuzzArg1
        ]


{-| Run a [fuzz test](#fuzz) using two random inputs.

This is a convenience function that lets you skip calling [`Fuzz.pair`](Fuzz#pair).

See [`fuzzWith`](#fuzzWith) for an example of writing this using tuples.

    import Test exposing (fuzz2)
    import Fuzz exposing (list, int)


    fuzz2 (list int) int "List.reverse never influences List.member" <|
        \nums target ->
            List.member target (List.reverse nums)
                |> Expect.equal (List.member target nums)

fuzz2: 
    Fuzz.Fuzzer a
    -> Fuzz.Fuzzer b
    -> String
    -> (a -> b -> Expect.Expectation)
    -> Test.Test
-}
fuzz2 :
    Elm.Expression
    -> Elm.Expression
    -> String
    -> (Elm.Expression -> Elm.Expression -> Elm.Expression)
    -> Elm.Expression
fuzz2 fuzz2Arg fuzz2Arg0 fuzz2Arg1 fuzz2Arg2 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test" ]
            , name = "fuzz2"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        , Type.string
                        , Type.function
                            [ Type.var "a", Type.var "b" ]
                            (Type.namedWith [ "Expect" ] "Expectation" [])
                        ]
                        (Type.namedWith [ "Test" ] "Test" [])
                    )
            }
        )
        [ fuzz2Arg
        , fuzz2Arg0
        , Elm.string fuzz2Arg1
        , Elm.functionReduced
            "fuzz2Unpack"
            (\functionReducedUnpack ->
                Elm.functionReduced "unpack" (fuzz2Arg2 functionReducedUnpack)
            )
        ]


{-| Run a [fuzz test](#fuzz) using three random inputs.

This is a convenience function that lets you skip calling [`Fuzz.triple`](Fuzz#triple).

fuzz3: 
    Fuzz.Fuzzer a
    -> Fuzz.Fuzzer b
    -> Fuzz.Fuzzer c
    -> String
    -> (a -> b -> c -> Expect.Expectation)
    -> Test.Test
-}
fuzz3 :
    Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> String
    -> (Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression)
    -> Elm.Expression
fuzz3 fuzz3Arg fuzz3Arg0 fuzz3Arg1 fuzz3Arg2 fuzz3Arg3 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test" ]
            , name = "fuzz3"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ]
                        , Type.string
                        , Type.function
                            [ Type.var "a", Type.var "b", Type.var "c" ]
                            (Type.namedWith [ "Expect" ] "Expectation" [])
                        ]
                        (Type.namedWith [ "Test" ] "Test" [])
                    )
            }
        )
        [ fuzz3Arg
        , fuzz3Arg0
        , fuzz3Arg1
        , Elm.string fuzz3Arg2
        , Elm.functionReduced
            "fuzz3Unpack"
            (\functionReducedUnpack ->
                Elm.functionReduced
                    "unpack"
                    (\functionReducedUnpack0 ->
                        Elm.functionReduced
                            "unpack"
                            (fuzz3Arg3 functionReducedUnpack
                                functionReducedUnpack0
                            )
                    )
            )
        ]


{-| Run a [`fuzz`](#fuzz) test with the given [`FuzzOptions`](#FuzzOptions).

Note that there is no `fuzzWith2`, but you can always pass more fuzz values in
using [`Fuzz.pair`](Fuzz#pair), [`Fuzz.triple`](Fuzz#triple),
for example like this:

    import Test exposing (fuzzWith)
    import Fuzz exposing (pair, list, int)
    import Expect


    fuzzWith { runs = 4200, distribution = noDistribution }
        (pair (list int) int)
        "List.reverse never influences List.member" <|
            \(nums, target) ->
                List.member target (List.reverse nums)
                    |> Expect.equal (List.member target nums)

fuzzWith: 
    Test.FuzzOptions a
    -> Fuzz.Fuzzer a
    -> String
    -> (a -> Expect.Expectation)
    -> Test.Test
-}
fuzzWith :
    Elm.Expression
    -> Elm.Expression
    -> String
    -> (Elm.Expression -> Elm.Expression)
    -> Elm.Expression
fuzzWith fuzzWithArg fuzzWithArg0 fuzzWithArg1 fuzzWithArg2 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test" ]
            , name = "fuzzWith"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Test" ]
                            "FuzzOptions"
                            [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.string
                        , Type.function
                            [ Type.var "a" ]
                            (Type.namedWith [ "Expect" ] "Expectation" [])
                        ]
                        (Type.namedWith [ "Test" ] "Test" [])
                    )
            }
        )
        [ fuzzWithArg
        , fuzzWithArg0
        , Elm.string fuzzWithArg1
        , Elm.functionReduced "fuzzWithUnpack" fuzzWithArg2
        ]


{-| Opts out of the test input distribution checking.

noDistribution: Test.Distribution a
-}
noDistribution : Elm.Expression
noDistribution =
    Elm.value
        { importFrom = [ "Test" ]
        , name = "noDistribution"
        , annotation =
            Just (Type.namedWith [ "Test" ] "Distribution" [ Type.var "a" ])
        }


{-| Collects statistics and reports them after the test runs (both when it passes
and fails).

reportDistribution: List ( String, a -> Bool ) -> Test.Distribution a
-}
reportDistribution : List Elm.Expression -> Elm.Expression
reportDistribution reportDistributionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test" ]
            , name = "reportDistribution"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.tuple
                                Type.string
                                (Type.function [ Type.var "a" ] Type.bool)
                            )
                        ]
                        (Type.namedWith
                            [ "Test" ]
                            "Distribution"
                            [ Type.var "a" ]
                        )
                    )
            }
        )
        [ Elm.list reportDistributionArg ]


{-| Collects statistics and makes sure the expected distribution is met.

Fails the test and reports the distribution if the expected distribution is not met.

Uses a statistical test to make sure the distribution doesn't pass or fail the
distribution by accident (a flaky test). Will run more tests than specified with the
`runs` config option if needed.

This has the consequence of running more tests the closer your expected distribution
is to the true distribution. You can thus minimize and speed up this
"making sure" process by requesting slightly less % of your distribution than
needed.

Currently the statistical test is tuned to allow a false positive/negative in
1 in every 10^9 tests.

expectDistribution: 
    List ( Test.Distribution.ExpectedDistribution, String, a -> Bool )
    -> Test.Distribution a
-}
expectDistribution : List Elm.Expression -> Elm.Expression
expectDistribution expectDistributionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test" ]
            , name = "expectDistribution"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.triple
                                (Type.namedWith
                                    [ "Test", "Distribution" ]
                                    "ExpectedDistribution"
                                    []
                                )
                                Type.string
                                (Type.function [ Type.var "a" ] Type.bool)
                            )
                        ]
                        (Type.namedWith
                            [ "Test" ]
                            "Distribution"
                            [ Type.var "a" ]
                        )
                    )
            }
        )
        [ Elm.list expectDistributionArg ]


annotation_ :
    { test : Type.Annotation
    , fuzzOptions : Type.Annotation -> Type.Annotation
    , distribution : Type.Annotation -> Type.Annotation
    }
annotation_ =
    { test =
        Type.alias
            moduleName_
            "Test"
            []
            (Type.namedWith [ "Test", "Internal" ] "Test" [])
    , fuzzOptions =
        \fuzzOptionsArg0 ->
            Type.alias
                moduleName_
                "FuzzOptions"
                [ fuzzOptionsArg0 ]
                (Type.record
                    [ ( "runs", Type.int )
                    , ( "distribution"
                      , Type.namedWith
                            [ "Test" ]
                            "Distribution"
                            [ Type.var "a" ]
                      )
                    ]
                )
    , distribution =
        \distributionArg0 ->
            Type.alias
                moduleName_
                "Distribution"
                [ distributionArg0 ]
                (Type.namedWith
                    [ "Test", "Distribution", "Internal" ]
                    "Distribution"
                    [ Type.var "a" ]
                )
    }


make_ :
    { fuzzOptions :
        { runs : Elm.Expression, distribution : Elm.Expression }
        -> Elm.Expression
    }
make_ =
    { fuzzOptions =
        \fuzzOptions_args ->
            Elm.withType
                (Type.alias
                    [ "Test" ]
                    "FuzzOptions"
                    [ Type.var "a" ]
                    (Type.record
                        [ ( "runs", Type.int )
                        , ( "distribution"
                          , Type.namedWith
                                [ "Test" ]
                                "Distribution"
                                [ Type.var "a" ]
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "runs" fuzzOptions_args.runs
                    , Tuple.pair "distribution" fuzzOptions_args.distribution
                    ]
                )
    }


call_ :
    { test : Elm.Expression -> Elm.Expression -> Elm.Expression
    , describe : Elm.Expression -> Elm.Expression -> Elm.Expression
    , concat : Elm.Expression -> Elm.Expression
    , todo : Elm.Expression -> Elm.Expression
    , skip : Elm.Expression -> Elm.Expression
    , only : Elm.Expression -> Elm.Expression
    , fuzz :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , fuzz2 :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , fuzz3 :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , fuzzWith :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , reportDistribution : Elm.Expression -> Elm.Expression
    , expectDistribution : Elm.Expression -> Elm.Expression
    }
call_ =
    { test =
        \testArg testArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test" ]
                    , name = "test"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.function
                                    [ Type.unit ]
                                    (Type.namedWith
                                        [ "Expect" ]
                                        "Expectation"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Test" ] "Test" [])
                            )
                    }
                )
                [ testArg, testArg0 ]
    , describe =
        \describeArg describeArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test" ]
                    , name = "describe"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.list
                                    (Type.namedWith [ "Test" ] "Test" [])
                                ]
                                (Type.namedWith [ "Test" ] "Test" [])
                            )
                    }
                )
                [ describeArg, describeArg0 ]
    , concat =
        \concatArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test" ]
                    , name = "concat"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [ "Test" ] "Test" [])
                                ]
                                (Type.namedWith [ "Test" ] "Test" [])
                            )
                    }
                )
                [ concatArg ]
    , todo =
        \todoArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test" ]
                    , name = "todo"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith [ "Test" ] "Test" [])
                            )
                    }
                )
                [ todoArg ]
    , skip =
        \skipArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test" ]
                    , name = "skip"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Test" ] "Test" [] ]
                                (Type.namedWith [ "Test" ] "Test" [])
                            )
                    }
                )
                [ skipArg ]
    , only =
        \onlyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test" ]
                    , name = "only"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Test" ] "Test" [] ]
                                (Type.namedWith [ "Test" ] "Test" [])
                            )
                    }
                )
                [ onlyArg ]
    , fuzz =
        \fuzzArg fuzzArg0 fuzzArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test" ]
                    , name = "fuzz"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                , Type.string
                                , Type.function
                                    [ Type.var "a" ]
                                    (Type.namedWith
                                        [ "Expect" ]
                                        "Expectation"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Test" ] "Test" [])
                            )
                    }
                )
                [ fuzzArg, fuzzArg0, fuzzArg1 ]
    , fuzz2 =
        \fuzz2Arg fuzz2Arg0 fuzz2Arg1 fuzz2Arg2 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test" ]
                    , name = "fuzz2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "b" ]
                                , Type.string
                                , Type.function
                                    [ Type.var "a", Type.var "b" ]
                                    (Type.namedWith
                                        [ "Expect" ]
                                        "Expectation"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Test" ] "Test" [])
                            )
                    }
                )
                [ fuzz2Arg, fuzz2Arg0, fuzz2Arg1, fuzz2Arg2 ]
    , fuzz3 =
        \fuzz3Arg fuzz3Arg0 fuzz3Arg1 fuzz3Arg2 fuzz3Arg3 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test" ]
                    , name = "fuzz3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "b" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "c" ]
                                , Type.string
                                , Type.function
                                    [ Type.var "a", Type.var "b", Type.var "c" ]
                                    (Type.namedWith
                                        [ "Expect" ]
                                        "Expectation"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Test" ] "Test" [])
                            )
                    }
                )
                [ fuzz3Arg, fuzz3Arg0, fuzz3Arg1, fuzz3Arg2, fuzz3Arg3 ]
    , fuzzWith =
        \fuzzWithArg fuzzWithArg0 fuzzWithArg1 fuzzWithArg2 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test" ]
                    , name = "fuzzWith"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Test" ]
                                    "FuzzOptions"
                                    [ Type.var "a" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                , Type.string
                                , Type.function
                                    [ Type.var "a" ]
                                    (Type.namedWith
                                        [ "Expect" ]
                                        "Expectation"
                                        []
                                    )
                                ]
                                (Type.namedWith [ "Test" ] "Test" [])
                            )
                    }
                )
                [ fuzzWithArg, fuzzWithArg0, fuzzWithArg1, fuzzWithArg2 ]
    , reportDistribution =
        \reportDistributionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test" ]
                    , name = "reportDistribution"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.tuple
                                        Type.string
                                        (Type.function
                                            [ Type.var "a" ]
                                            Type.bool
                                        )
                                    )
                                ]
                                (Type.namedWith
                                    [ "Test" ]
                                    "Distribution"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ reportDistributionArg ]
    , expectDistribution =
        \expectDistributionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test" ]
                    , name = "expectDistribution"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.triple
                                        (Type.namedWith
                                            [ "Test", "Distribution" ]
                                            "ExpectedDistribution"
                                            []
                                        )
                                        Type.string
                                        (Type.function
                                            [ Type.var "a" ]
                                            Type.bool
                                        )
                                    )
                                ]
                                (Type.namedWith
                                    [ "Test" ]
                                    "Distribution"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ expectDistributionArg ]
    }


values_ :
    { test : Elm.Expression
    , describe : Elm.Expression
    , concat : Elm.Expression
    , todo : Elm.Expression
    , skip : Elm.Expression
    , only : Elm.Expression
    , fuzz : Elm.Expression
    , fuzz2 : Elm.Expression
    , fuzz3 : Elm.Expression
    , fuzzWith : Elm.Expression
    , noDistribution : Elm.Expression
    , reportDistribution : Elm.Expression
    , expectDistribution : Elm.Expression
    }
values_ =
    { test =
        Elm.value
            { importFrom = [ "Test" ]
            , name = "test"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.function
                            [ Type.unit ]
                            (Type.namedWith [ "Expect" ] "Expectation" [])
                        ]
                        (Type.namedWith [ "Test" ] "Test" [])
                    )
            }
    , describe =
        Elm.value
            { importFrom = [ "Test" ]
            , name = "describe"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.list (Type.namedWith [ "Test" ] "Test" [])
                        ]
                        (Type.namedWith [ "Test" ] "Test" [])
                    )
            }
    , concat =
        Elm.value
            { importFrom = [ "Test" ]
            , name = "concat"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.namedWith [ "Test" ] "Test" []) ]
                        (Type.namedWith [ "Test" ] "Test" [])
                    )
            }
    , todo =
        Elm.value
            { importFrom = [ "Test" ]
            , name = "todo"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Test" ] "Test" [])
                    )
            }
    , skip =
        Elm.value
            { importFrom = [ "Test" ]
            , name = "skip"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Test" ] "Test" [] ]
                        (Type.namedWith [ "Test" ] "Test" [])
                    )
            }
    , only =
        Elm.value
            { importFrom = [ "Test" ]
            , name = "only"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Test" ] "Test" [] ]
                        (Type.namedWith [ "Test" ] "Test" [])
                    )
            }
    , fuzz =
        Elm.value
            { importFrom = [ "Test" ]
            , name = "fuzz"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.string
                        , Type.function
                            [ Type.var "a" ]
                            (Type.namedWith [ "Expect" ] "Expectation" [])
                        ]
                        (Type.namedWith [ "Test" ] "Test" [])
                    )
            }
    , fuzz2 =
        Elm.value
            { importFrom = [ "Test" ]
            , name = "fuzz2"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        , Type.string
                        , Type.function
                            [ Type.var "a", Type.var "b" ]
                            (Type.namedWith [ "Expect" ] "Expectation" [])
                        ]
                        (Type.namedWith [ "Test" ] "Test" [])
                    )
            }
    , fuzz3 =
        Elm.value
            { importFrom = [ "Test" ]
            , name = "fuzz3"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ]
                        , Type.string
                        , Type.function
                            [ Type.var "a", Type.var "b", Type.var "c" ]
                            (Type.namedWith [ "Expect" ] "Expectation" [])
                        ]
                        (Type.namedWith [ "Test" ] "Test" [])
                    )
            }
    , fuzzWith =
        Elm.value
            { importFrom = [ "Test" ]
            , name = "fuzzWith"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Test" ]
                            "FuzzOptions"
                            [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.string
                        , Type.function
                            [ Type.var "a" ]
                            (Type.namedWith [ "Expect" ] "Expectation" [])
                        ]
                        (Type.namedWith [ "Test" ] "Test" [])
                    )
            }
    , noDistribution =
        Elm.value
            { importFrom = [ "Test" ]
            , name = "noDistribution"
            , annotation =
                Just (Type.namedWith [ "Test" ] "Distribution" [ Type.var "a" ])
            }
    , reportDistribution =
        Elm.value
            { importFrom = [ "Test" ]
            , name = "reportDistribution"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.tuple
                                Type.string
                                (Type.function [ Type.var "a" ] Type.bool)
                            )
                        ]
                        (Type.namedWith
                            [ "Test" ]
                            "Distribution"
                            [ Type.var "a" ]
                        )
                    )
            }
    , expectDistribution =
        Elm.value
            { importFrom = [ "Test" ]
            , name = "expectDistribution"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.triple
                                (Type.namedWith
                                    [ "Test", "Distribution" ]
                                    "ExpectedDistribution"
                                    []
                                )
                                Type.string
                                (Type.function [ Type.var "a" ] Type.bool)
                            )
                        ]
                        (Type.namedWith
                            [ "Test" ]
                            "Distribution"
                            [ Type.var "a" ]
                        )
                    )
            }
    }


