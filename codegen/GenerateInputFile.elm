module GenerateInputFile exposing (main)

import Elm
import Gen.CodeGen.Generate as Generate
import Json.Decode exposing (string)


main =
    Generate.fromJson string (\input -> [ file input ])


file : String -> Elm.File
file input =
    Elm.file [ "Input" ]
        [ Elm.declaration "testInput"
            (Elm.string "replace me with the test input")
          , Elm.declaration "expectedA"
            (Elm.string "")
          , Elm.declaration "expectedB"
            (Elm.string "")
          , Elm.declaration "input"
            (Elm.string input)
        ]