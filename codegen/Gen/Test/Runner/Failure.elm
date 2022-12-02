module Gen.Test.Runner.Failure exposing (annotation_, caseOf_, make_, moduleName_)

{-| 
@docs caseOf_, make_, annotation_, moduleName_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Test", "Runner", "Failure" ]


annotation_ : { reason : Type.Annotation, invalidReason : Type.Annotation }
annotation_ =
    { reason = Type.namedWith [ "Test", "Runner", "Failure" ] "Reason" []
    , invalidReason =
        Type.namedWith [ "Test", "Runner", "Failure" ] "InvalidReason" []
    }


make_ :
    { custom : Elm.Expression
    , equality : Elm.Expression -> Elm.Expression -> Elm.Expression
    , comparison : Elm.Expression -> Elm.Expression -> Elm.Expression
    , listDiff : Elm.Expression -> Elm.Expression -> Elm.Expression
    , collectionDiff : Elm.Expression -> Elm.Expression
    , tODO : Elm.Expression
    , invalid : Elm.Expression -> Elm.Expression
    , emptyList : Elm.Expression
    , nonpositiveFuzzCount : Elm.Expression
    , invalidFuzzer : Elm.Expression
    , badDescription : Elm.Expression
    , duplicatedName : Elm.Expression
    , distributionInsufficient : Elm.Expression
    , distributionBug : Elm.Expression
    }
make_ =
    { custom =
        Elm.value
            { importFrom = [ "Test", "Runner", "Failure" ]
            , name = "Custom"
            , annotation = Just (Type.namedWith [] "Reason" [])
            }
    , equality =
        \ar0 ar1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Runner", "Failure" ]
                    , name = "Equality"
                    , annotation = Just (Type.namedWith [] "Reason" [])
                    }
                )
                [ ar0, ar1 ]
    , comparison =
        \ar0 ar1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Runner", "Failure" ]
                    , name = "Comparison"
                    , annotation = Just (Type.namedWith [] "Reason" [])
                    }
                )
                [ ar0, ar1 ]
    , listDiff =
        \ar0 ar1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Runner", "Failure" ]
                    , name = "ListDiff"
                    , annotation = Just (Type.namedWith [] "Reason" [])
                    }
                )
                [ ar0, ar1 ]
    , collectionDiff =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Runner", "Failure" ]
                    , name = "CollectionDiff"
                    , annotation = Just (Type.namedWith [] "Reason" [])
                    }
                )
                [ ar0 ]
    , tODO =
        Elm.value
            { importFrom = [ "Test", "Runner", "Failure" ]
            , name = "TODO"
            , annotation = Just (Type.namedWith [] "Reason" [])
            }
    , invalid =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Runner", "Failure" ]
                    , name = "Invalid"
                    , annotation = Just (Type.namedWith [] "Reason" [])
                    }
                )
                [ ar0 ]
    , emptyList =
        Elm.value
            { importFrom = [ "Test", "Runner", "Failure" ]
            , name = "EmptyList"
            , annotation = Just (Type.namedWith [] "InvalidReason" [])
            }
    , nonpositiveFuzzCount =
        Elm.value
            { importFrom = [ "Test", "Runner", "Failure" ]
            , name = "NonpositiveFuzzCount"
            , annotation = Just (Type.namedWith [] "InvalidReason" [])
            }
    , invalidFuzzer =
        Elm.value
            { importFrom = [ "Test", "Runner", "Failure" ]
            , name = "InvalidFuzzer"
            , annotation = Just (Type.namedWith [] "InvalidReason" [])
            }
    , badDescription =
        Elm.value
            { importFrom = [ "Test", "Runner", "Failure" ]
            , name = "BadDescription"
            , annotation = Just (Type.namedWith [] "InvalidReason" [])
            }
    , duplicatedName =
        Elm.value
            { importFrom = [ "Test", "Runner", "Failure" ]
            , name = "DuplicatedName"
            , annotation = Just (Type.namedWith [] "InvalidReason" [])
            }
    , distributionInsufficient =
        Elm.value
            { importFrom = [ "Test", "Runner", "Failure" ]
            , name = "DistributionInsufficient"
            , annotation = Just (Type.namedWith [] "InvalidReason" [])
            }
    , distributionBug =
        Elm.value
            { importFrom = [ "Test", "Runner", "Failure" ]
            , name = "DistributionBug"
            , annotation = Just (Type.namedWith [] "InvalidReason" [])
            }
    }


caseOf_ :
    { reason :
        Elm.Expression
        -> { reasonTags_0_0
            | custom : Elm.Expression
            , equality : Elm.Expression -> Elm.Expression -> Elm.Expression
            , comparison : Elm.Expression -> Elm.Expression -> Elm.Expression
            , listDiff : Elm.Expression -> Elm.Expression -> Elm.Expression
            , collectionDiff : Elm.Expression -> Elm.Expression
            , tODO : Elm.Expression
            , invalid : Elm.Expression -> Elm.Expression
        }
        -> Elm.Expression
    , invalidReason :
        Elm.Expression
        -> { invalidReasonTags_1_0
            | emptyList : Elm.Expression
            , nonpositiveFuzzCount : Elm.Expression
            , invalidFuzzer : Elm.Expression
            , badDescription : Elm.Expression
            , duplicatedName : Elm.Expression
            , distributionInsufficient : Elm.Expression
            , distributionBug : Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { reason =
        \reasonExpression reasonTags ->
            Elm.Case.custom
                reasonExpression
                (Type.namedWith [ "Test", "Runner", "Failure" ] "Reason" [])
                [ Elm.Case.branch0 "Custom" reasonTags.custom
                , Elm.Case.branch2
                    "Equality"
                    ( "string.String", Type.string )
                    ( "string.String", Type.string )
                    reasonTags.equality
                , Elm.Case.branch2
                    "Comparison"
                    ( "string.String", Type.string )
                    ( "string.String", Type.string )
                    reasonTags.comparison
                , Elm.Case.branch2
                    "ListDiff"
                    ( "list.List", Type.list Type.string )
                    ( "list.List", Type.list Type.string )
                    reasonTags.listDiff
                , Elm.Case.branch1
                    "CollectionDiff"
                    ( "one"
                    , Type.record
                        [ ( "expected", Type.string )
                        , ( "actual", Type.string )
                        , ( "extra", Type.list Type.string )
                        , ( "missing", Type.list Type.string )
                        ]
                    )
                    reasonTags.collectionDiff
                , Elm.Case.branch0 "TODO" reasonTags.tODO
                , Elm.Case.branch1
                    "Invalid"
                    ( "test.Runner.Failure.InvalidReason"
                    , Type.namedWith
                        [ "Test", "Runner", "Failure" ]
                        "InvalidReason"
                        []
                    )
                    reasonTags.invalid
                ]
    , invalidReason =
        \invalidReasonExpression invalidReasonTags ->
            Elm.Case.custom
                invalidReasonExpression
                (Type.namedWith
                    [ "Test", "Runner", "Failure" ]
                    "InvalidReason"
                    []
                )
                [ Elm.Case.branch0 "EmptyList" invalidReasonTags.emptyList
                , Elm.Case.branch0
                    "NonpositiveFuzzCount"
                    invalidReasonTags.nonpositiveFuzzCount
                , Elm.Case.branch0
                    "InvalidFuzzer"
                    invalidReasonTags.invalidFuzzer
                , Elm.Case.branch0
                    "BadDescription"
                    invalidReasonTags.badDescription
                , Elm.Case.branch0
                    "DuplicatedName"
                    invalidReasonTags.duplicatedName
                , Elm.Case.branch0
                    "DistributionInsufficient"
                    invalidReasonTags.distributionInsufficient
                , Elm.Case.branch0
                    "DistributionBug"
                    invalidReasonTags.distributionBug
                ]
    }


