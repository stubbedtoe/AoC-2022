module GenerateSolutionFile exposing (main)

import Elm
import Elm.Case
import Elm.Declare
import Elm.Annotation as Type
import Gen.CodeGen.Generate as Generate
import Gen.Input

main : Program {} () ()
main =
    Generate.run [ file ]


file : Elm.File
file =
    let
        solveA =
            Elm.Declare.fn "solveA"
                ( "input", Just Type.string )
                (\_ ->
                    Elm.string "implement partA here"
                )
        solveB =
            Elm.Declare.fn "solveB"
                ( "input", Just Type.string )
                (\_ ->
                    Elm.string "implement partB here"
                )
    in
    Elm.file [ "Solution" ]
        [ 
            solveA.declaration
            , solveB.declaration
            , Elm.declaration "partA" <|
                Elm.fn ("inputType", Just Type.string )
                (\inputType ->
                    Elm.Case.string inputType
                        { cases = [
                            ("test", solveA.call Gen.Input.testInput)
                            , ("input", solveA.call Gen.Input.input)
                        ]
                        , otherwise = Elm.string "supported args are \"test\" or \"input\""
                        }
                )
            , Elm.declaration "partB" <|
                Elm.fn ("inputType", Just Type.string )
                (\inputType ->
                    Elm.Case.string inputType
                        { cases = [
                            ("test", solveB.call Gen.Input.testInput)
                            , ("input", solveB.call Gen.Input.input)
                        ]
                        , otherwise = Elm.string "supported args are \"test\" or \"input\""
                        }
                )
        ]
