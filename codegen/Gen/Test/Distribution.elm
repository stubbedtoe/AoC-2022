module Gen.Test.Distribution exposing (annotation_, atLeast, call_, caseOf_, distributionReportTable, make_, moduleName_, moreThanZero, values_, zero)

{-| 
@docs values_, call_, caseOf_, make_, annotation_, distributionReportTable, moreThanZero, zero, atLeast, moduleName_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Test", "Distribution" ]


{-| A requirement that a given value class should happen at least N% of the time
in a given test.

The example below says that at least 30% of the fuzz test inputs should be
multiples of 3.

    fuzzWith
        { runs = 10000
        , distribution =
            expectDistribution
                [ ( atLeast 30, "multiple of 3", \n -> (n |> modBy 3) == 0 )
                ]
        }

atLeast: Float -> Test.Distribution.ExpectedDistribution
-}
atLeast : Float -> Elm.Expression
atLeast atLeastArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Distribution" ]
            , name = "atLeast"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Test", "Distribution" ]
                            "ExpectedDistribution"
                            []
                        )
                    )
            }
        )
        [ Elm.float atLeastArg ]


{-| A requirement that a given value class should never happen in a given test.

zero: Test.Distribution.ExpectedDistribution
-}
zero : Elm.Expression
zero =
    Elm.value
        { importFrom = [ "Test", "Distribution" ]
        , name = "zero"
        , annotation =
            Just
                (Type.namedWith
                    [ "Test", "Distribution" ]
                    "ExpectedDistribution"
                    []
                )
        }


{-| A requirement that a given value class should happen at least once in a given
test.

moreThanZero: Test.Distribution.ExpectedDistribution
-}
moreThanZero : Elm.Expression
moreThanZero =
    Elm.value
        { importFrom = [ "Test", "Distribution" ]
        , name = "moreThanZero"
        , annotation =
            Just
                (Type.namedWith
                    [ "Test", "Distribution" ]
                    "ExpectedDistribution"
                    []
                )
        }


{-| Prettyprints the record inside `DistributionReport` into a table with histograms.

distributionReportTable: 
    { a | runsElapsed : Int, distributionCount : Dict.Dict (List String) Int }
    -> String
-}
distributionReportTable :
    { a | runsElapsed : Int, distributionCount : Elm.Expression }
    -> Elm.Expression
distributionReportTable distributionReportTableArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Distribution" ]
            , name = "distributionReportTable"
            , annotation =
                Just
                    (Type.function
                        [ Type.extensible
                            "a"
                            [ ( "runsElapsed", Type.int )
                            , ( "distributionCount"
                              , Type.namedWith
                                    [ "Dict" ]
                                    "Dict"
                                    [ Type.list Type.string, Type.int ]
                              )
                            ]
                        ]
                        Type.string
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair
                "runsElapsed"
                (Elm.int distributionReportTableArg.runsElapsed)
            , Tuple.pair
                "distributionCount"
                distributionReportTableArg.distributionCount
            ]
        ]


annotation_ :
    { expectedDistribution : Type.Annotation
    , distributionReport : Type.Annotation
    }
annotation_ =
    { expectedDistribution =
        Type.alias
            moduleName_
            "ExpectedDistribution"
            []
            (Type.namedWith
                [ "Test", "Distribution", "Internal" ]
                "ExpectedDistribution"
                []
            )
    , distributionReport =
        Type.namedWith [ "Test", "Distribution" ] "DistributionReport" []
    }


make_ :
    { noDistribution : Elm.Expression
    , distributionToReport : Elm.Expression -> Elm.Expression
    , distributionCheckSucceeded : Elm.Expression -> Elm.Expression
    , distributionCheckFailed : Elm.Expression -> Elm.Expression
    }
make_ =
    { noDistribution =
        Elm.value
            { importFrom = [ "Test", "Distribution" ]
            , name = "NoDistribution"
            , annotation = Just (Type.namedWith [] "DistributionReport" [])
            }
    , distributionToReport =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Distribution" ]
                    , name = "DistributionToReport"
                    , annotation =
                        Just (Type.namedWith [] "DistributionReport" [])
                    }
                )
                [ ar0 ]
    , distributionCheckSucceeded =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Distribution" ]
                    , name = "DistributionCheckSucceeded"
                    , annotation =
                        Just (Type.namedWith [] "DistributionReport" [])
                    }
                )
                [ ar0 ]
    , distributionCheckFailed =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Distribution" ]
                    , name = "DistributionCheckFailed"
                    , annotation =
                        Just (Type.namedWith [] "DistributionReport" [])
                    }
                )
                [ ar0 ]
    }


caseOf_ :
    { distributionReport :
        Elm.Expression
        -> { distributionReportTags_0_0
            | noDistribution : Elm.Expression
            , distributionToReport : Elm.Expression -> Elm.Expression
            , distributionCheckSucceeded : Elm.Expression -> Elm.Expression
            , distributionCheckFailed : Elm.Expression -> Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { distributionReport =
        \distributionReportExpression distributionReportTags ->
            Elm.Case.custom
                distributionReportExpression
                (Type.namedWith
                    [ "Test", "Distribution" ]
                    "DistributionReport"
                    []
                )
                [ Elm.Case.branch0
                    "NoDistribution"
                    distributionReportTags.noDistribution
                , Elm.Case.branch1
                    "DistributionToReport"
                    ( "one"
                    , Type.record
                        [ ( "distributionCount"
                          , Type.namedWith
                                [ "Dict" ]
                                "Dict"
                                [ Type.list Type.string, Type.int ]
                          )
                        , ( "runsElapsed", Type.int )
                        ]
                    )
                    distributionReportTags.distributionToReport
                , Elm.Case.branch1
                    "DistributionCheckSucceeded"
                    ( "one"
                    , Type.record
                        [ ( "distributionCount"
                          , Type.namedWith
                                [ "Dict" ]
                                "Dict"
                                [ Type.list Type.string, Type.int ]
                          )
                        , ( "runsElapsed", Type.int )
                        ]
                    )
                    distributionReportTags.distributionCheckSucceeded
                , Elm.Case.branch1
                    "DistributionCheckFailed"
                    ( "one"
                    , Type.record
                        [ ( "distributionCount"
                          , Type.namedWith
                                [ "Dict" ]
                                "Dict"
                                [ Type.list Type.string, Type.int ]
                          )
                        , ( "runsElapsed", Type.int )
                        , ( "badLabel", Type.string )
                        , ( "badLabelPercentage", Type.float )
                        , ( "expectedDistribution", Type.string )
                        ]
                    )
                    distributionReportTags.distributionCheckFailed
                ]
    }


call_ :
    { atLeast : Elm.Expression -> Elm.Expression
    , distributionReportTable : Elm.Expression -> Elm.Expression
    }
call_ =
    { atLeast =
        \atLeastArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Distribution" ]
                    , name = "atLeast"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Test", "Distribution" ]
                                    "ExpectedDistribution"
                                    []
                                )
                            )
                    }
                )
                [ atLeastArg ]
    , distributionReportTable =
        \distributionReportTableArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Distribution" ]
                    , name = "distributionReportTable"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.extensible
                                    "a"
                                    [ ( "runsElapsed", Type.int )
                                    , ( "distributionCount"
                                      , Type.namedWith
                                            [ "Dict" ]
                                            "Dict"
                                            [ Type.list Type.string, Type.int ]
                                      )
                                    ]
                                ]
                                Type.string
                            )
                    }
                )
                [ distributionReportTableArg ]
    }


values_ :
    { atLeast : Elm.Expression
    , zero : Elm.Expression
    , moreThanZero : Elm.Expression
    , distributionReportTable : Elm.Expression
    }
values_ =
    { atLeast =
        Elm.value
            { importFrom = [ "Test", "Distribution" ]
            , name = "atLeast"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Test", "Distribution" ]
                            "ExpectedDistribution"
                            []
                        )
                    )
            }
    , zero =
        Elm.value
            { importFrom = [ "Test", "Distribution" ]
            , name = "zero"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Test", "Distribution" ]
                        "ExpectedDistribution"
                        []
                    )
            }
    , moreThanZero =
        Elm.value
            { importFrom = [ "Test", "Distribution" ]
            , name = "moreThanZero"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Test", "Distribution" ]
                        "ExpectedDistribution"
                        []
                    )
            }
    , distributionReportTable =
        Elm.value
            { importFrom = [ "Test", "Distribution" ]
            , name = "distributionReportTable"
            , annotation =
                Just
                    (Type.function
                        [ Type.extensible
                            "a"
                            [ ( "runsElapsed", Type.int )
                            , ( "distributionCount"
                              , Type.namedWith
                                    [ "Dict" ]
                                    "Dict"
                                    [ Type.list Type.string, Type.int ]
                              )
                            ]
                        ]
                        Type.string
                    )
            }
    }


