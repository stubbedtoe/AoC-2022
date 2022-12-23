port module Main exposing (main)

import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import Json.Decode as D
import Json.Encode as E
import List.Extra
import Platform exposing (Program)
import Solutions.Day15.Solution as Blackbox


port get : (String -> msg) -> Sub msg


port put : String -> Cmd msg


port sendFileName : E.Value -> Cmd msg


port receiveData : (E.Value -> msg) -> Sub msg


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { residualCommand : String, fileContents : Maybe String }


type Msg
    = Input String
    | ReceivedDataFromJS E.Value


type alias Flags =
    ()


init : () -> ( Model, Cmd Msg )
init _ =
    { residualCommand = "", fileContents = Nothing } |> withNoCmd


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ get Input, receiveData ReceivedDataFromJS ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input input ->
            case input == "" of
                True ->
                    model |> withNoCmd

                False ->
                    processCommand { model | residualCommand = getResidualCmd input } input

        ReceivedDataFromJS value ->
            case decodeFileContents value of
                Nothing ->
                    model |> withCmd (put "Couldn't load file")

                Just data ->
                    let
                        input =
                            -- If there is a residual command, prepend it to the
                            -- input before sending the input to the black box.
                            case model.residualCommand == "" of
                                True ->
                                    removeComments data

                                False ->
                                    ":" ++ model.residualCommand ++ " " ++ removeComments data
                    in
                    { model | fileContents = Just data } |> withCmd (put <| Blackbox.partA input)


processCommand : Model -> String -> ( Model, Cmd Msg )
processCommand model cmdString =
    let
        args =
            String.split " " cmdString
                |> List.map String.trim
                |> List.filter (\item -> item /= "")



        input = 
            args
                |> List.drop 1
                |> String.join ""

        cmd =
            List.head args
    in
    case cmd of
    
        Just "partA" ->
            model |> withCmd (put (Blackbox.partA <| input))

        Just "partB" ->
            model |> withCmd (put (Blackbox.partB <| input))

        _ ->
            model |> withNoCmd



decodeFileContents : E.Value -> Maybe String
decodeFileContents value =
    case D.decodeValue D.string value of
        Ok str ->
            Just str

        Err _ ->
            Nothing



-- HELPERS


{-| This is used in the context

:get FILENAME xxx yyy zzz

in which xxx yyy zzzz is the command to be
applied to the contents of FILENAME once
it is received.

-}
getResidualCmd : String -> String
getResidualCmd input =
    let
        args =
            input
                |> String.split " "
                |> List.filter (\s -> s /= "")
    in
    args
        |> List.drop 2
        |> String.join " "



-- FILE/CONTENT OPERATIONS


removeComments : String -> String
removeComments input =
    input
        |> String.lines
        |> List.filter (\line -> String.left 1 line /= "#")
        |> String.join "\n"
        |> String.trim


headOfFile model =
    model.fileContents
        |> Maybe.map (head 5)
        |> Maybe.withDefault "no file loaded"


tailOfFile model =
    model.fileContents
        |> Maybe.map (tail 5)
        |> Maybe.withDefault "no file loaded"


head : Int -> String -> String
head k input =
    input
        |> String.lines
        |> List.take k
        |> String.join "\n"


tail : Int -> String -> String
tail k input =
    let
        lines =
            String.lines input

        n =
            List.length lines
    in
    lines
        |> List.drop (n - k - 1)
        |> String.join "\n"
