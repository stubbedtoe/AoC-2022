module GenerateTestFile exposing (main)

import Elm
import Gen.Test
import Gen.Expect
import Gen.Solution
import Gen.Input
import Gen.CodeGen.Generate as Generate

main : Program {} () ()
main =
    Generate.run [ file ]

file : Elm.File
file =
    Elm.file [ "Test" ]
        [
              Elm.declaration "suite"
                    (Gen.Test.describe "Test" [
                        Gen.Test.describe "partA" [
                            Gen.Test.test "returns expected answer"
                            (\_ -> Gen.Expect.equal ( Gen.Solution.partA "test" ) ( Gen.Input.expectedA ) )
                        ]
                        , Gen.Test.describe "partB" [
                            Gen.Test.test "returns expected answer"
                            (\_ -> Gen.Expect.equal ( Gen.Solution.partB "test" ) ( Gen.Input.expectedB ) )
                        ]
                    ])
        ]