module Solutions.Day4.Test exposing (..)

{-| 
-}


import Expect
import Solutions.Day4.Input as Input
import Solutions.Day4.Solution as Solution
import Test


suite : Test.Test
suite =
    Test.describe
        "Test"
        [ Test.describe
            "partA"
            [ Test.test
                "returns expected answer"
                (\testUnpack ->
                    Expect.equal (Solution.partA "test") Input.expectedA
                )
            ]
        , Test.describe
            "partB"
            [ Test.test
                "returns expected answer"
                (\testUnpack ->
                    Expect.equal (Solution.partB "test") Input.expectedB
                )
            ]
        ]


