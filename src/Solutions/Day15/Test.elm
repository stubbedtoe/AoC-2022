module Solutions.Day15.Test exposing (..)

{-| 
-}


import Expect
import Solutions.Day15.Input as Input
import Solutions.Day15.Solution as Solution
import Test


suite : Test.Test
suite =
    Test.describe
        "Test"
        [ Test.describe
            "partA"
            [ Test.test
                "returns expected answer"
                (\_ ->
                    Expect.equal (Solution.partA "test") Input.expectedA
                )
            ]
        , Test.describe
            "partB"
            [ Test.test
                "returns expected answer"
                (\_ ->
                    Expect.equal (Solution.partB "test") Input.expectedB
                )
            ]
        ]


