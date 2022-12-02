module Gen.Fuzz exposing (andMap, andThen, annotation_, array, asciiChar, asciiString, asciiStringOfLength, asciiStringOfLengthBetween, bool, call_, char, constant, examples, filter, float, floatAtLeast, floatAtMost, floatRange, frequency, frequencyValues, fromGenerator, int, intAtLeast, intAtMost, intRange, invalid, labelExamples, lazy, list, listOfLength, listOfLengthBetween, map, map2, map3, map4, map5, map6, map7, map8, maybe, moduleName_, niceFloat, oneOf, oneOfValues, order, pair, percentage, result, sequence, shuffledList, string, stringOfLength, stringOfLengthBetween, traverse, triple, uniformInt, unit, values_, weightedBool)

{-| 
@docs values_, call_, annotation_, fromGenerator, traverse, sequence, lazy, andThen, andMap, map8, map7, map6, map5, map4, map3, map2, map, filter, invalid, constant, frequencyValues, frequency, oneOfValues, oneOf, weightedBool, order, unit, bool, result, maybe, array, shuffledList, listOfLengthBetween, listOfLength, list, triple, pair, asciiStringOfLengthBetween, asciiStringOfLength, asciiString, stringOfLengthBetween, stringOfLength, string, asciiChar, char, floatAtMost, floatAtLeast, floatRange, percentage, niceFloat, float, intAtMost, intAtLeast, uniformInt, intRange, int, labelExamples, examples, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Fuzz" ]


{-| Generate a few example values from the fuzzer.

Useful in REPL:

    > import Fuzz
    > Fuzz.examples 20 (Fuzz.intRange 20 50)
    [42,45,32,26,33,29,41,45,23,45,34,23,22,42,29,27,41,43,30,50]
        : List Int

Uses the first argument as the seed as well as the count of examples to generate.

Will return an empty list in case of rejection.

examples: Int -> Fuzz.Fuzzer a -> List a
-}
examples : Int -> Elm.Expression -> Elm.Expression
examples examplesArg examplesArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "examples"
            , annotation =
                Just
                    (Type.function
                        [ Type.int
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        ]
                        (Type.list (Type.var "a"))
                    )
            }
        )
        [ Elm.int examplesArg, examplesArg0 ]


{-| Show examples of values satisfying given classification predicates (see
also [`Test.reportDistribution`](Test#reportDistribution) and
[`Test.expectDistribution`](Test#expectDistribution)).

Generates a given number of values and classifies them based on the predicates.

Uses the first argument as the seed as well as the count of examples to
generate.

This function will always return all the given "base" labels, even if no
examples of them could be found:

    Fuzz.labelExamples 100
        [ ( "Lower boundary (1)", \n -> n == 1 )
        , ( "Upper boundary (20)", \n -> n == 20 )
        , ( "In the middle (2..19)", \n -> n > 1 && n < 20 )
        , ( "Outside boundaries??", \n -> n < 1 || n > 20 )
        ]
        (Fuzz.intRange 1 20)

    -->
    [ ( [ "Lower boundary (1)" ], Just 1 )
    , ( [ "Upper boundary (20)" ], Just 20 )
    , ( [ "In the middle (2..19)" ], Just 5 )
    , ( [ "Outside boundaries??" ], Nothing )
    ]

In case of predicate overlap (eg. something is both green and big) this
function will also return all the found combinations:

    Fuzz.labelExamples 100
        [ ( "fizz", \n -> (n |> modBy 3) == 0 )
        , ( "buzz", \n -> (n |> modBy 5) == 0 )
        ]
        (Fuzz.intRange 1 20)

    -->
    [ ( [ "fizz" ], Just 3 )
    , ( [ "buzz" ], Just 10 )
    , ( [ "fizz, buzz" ], Just 15 )
    ]

labelExamples: 
    Int
    -> List ( String, a -> Bool )
    -> Fuzz.Fuzzer a
    -> List ( List String, Maybe a )
-}
labelExamples : Int -> List Elm.Expression -> Elm.Expression -> Elm.Expression
labelExamples labelExamplesArg labelExamplesArg0 labelExamplesArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "labelExamples"
            , annotation =
                Just
                    (Type.function
                        [ Type.int
                        , Type.list
                            (Type.tuple
                                Type.string
                                (Type.function [ Type.var "a" ] Type.bool)
                            )
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        ]
                        (Type.list
                            (Type.tuple
                                (Type.list Type.string)
                                (Type.maybe (Type.var "a"))
                            )
                        )
                    )
            }
        )
        [ Elm.int labelExamplesArg
        , Elm.list labelExamplesArg0
        , labelExamplesArg1
        ]


{-| A fuzzer for int values. It will never produce `NaN`, `Infinity`, or
`-Infinity`.

This fuzzer will generate values in the range `Random.minInt .. Random.maxInt`.

  - Simplifies towards 0
  - Prefers positive values over negative ones
  - Prefers smaller values over larger ones

int: Fuzz.Fuzzer Int
-}
int : Elm.Expression
int =
    Elm.value
        { importFrom = [ "Fuzz" ]
        , name = "int"
        , annotation = Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.int ])
        }


{-| A fuzzer for int values between a given minimum and maximum value,
inclusive. Shrunk values will also be within the range.

intRange: Int -> Int -> Fuzz.Fuzzer Int
-}
intRange : Int -> Int -> Elm.Expression
intRange intRangeArg intRangeArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "intRange"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.int ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.int ])
                    )
            }
        )
        [ Elm.int intRangeArg, Elm.int intRangeArg0 ]


{-| Draw an integer between 0 and n inclusive.

Will simplify towards 0, but draws uniformly over the whole range.

Max supported value is 2^32 - 1.

uniformInt: Int -> Fuzz.Fuzzer Int
-}
uniformInt : Int -> Elm.Expression
uniformInt uniformIntArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "uniformInt"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.int ])
                    )
            }
        )
        [ Elm.int uniformIntArg ]


{-| A fuzzer that will generate values in range n..2^32-1.

intAtLeast: Int -> Fuzz.Fuzzer Int
-}
intAtLeast : Int -> Elm.Expression
intAtLeast intAtLeastArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "intAtLeast"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.int ])
                    )
            }
        )
        [ Elm.int intAtLeastArg ]


{-| A fuzzer that will generate values in range -(2^32-1)..n.

intAtMost: Int -> Fuzz.Fuzzer Int
-}
intAtMost : Int -> Elm.Expression
intAtMost intAtMostArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "intAtMost"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.int ])
                    )
            }
        )
        [ Elm.int intAtMostArg ]


{-| A fuzzer for float values.

Will prefer integer values, nice fractions and positive numbers over the rest.

Will occasionally try infinities and NaN. If you don't want to generate these,
use [`Fuzz.niceFloat`](#niceFloat).

float: Fuzz.Fuzzer Float
-}
float : Elm.Expression
float =
    Elm.value
        { importFrom = [ "Fuzz" ]
        , name = "float"
        , annotation = Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.float ])
        }


{-| A fuzzer for float values.

Will prefer integer values, nice fractions and positive numbers over the rest.

Will never try infinities or NaN.

niceFloat: Fuzz.Fuzzer Float
-}
niceFloat : Elm.Expression
niceFloat =
    Elm.value
        { importFrom = [ "Fuzz" ]
        , name = "niceFloat"
        , annotation = Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.float ])
        }


{-| A fuzzer for percentage values. Generates random floats between `0.0`
inclusive and `1.0` exclusive, in an uniform fashion.

Will occasionally try the boundaries.

Doesn't shrink to nice values like [`Fuzz.float`](#float) does; shrinks towards
zero.

percentage: Fuzz.Fuzzer Float
-}
percentage : Elm.Expression
percentage =
    Elm.value
        { importFrom = [ "Fuzz" ]
        , name = "percentage"
        , annotation = Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.float ])
        }


{-| A fuzzer for float values within between a given minimum and maximum (inclusive).

Shrunken values will also be within the range.

floatRange: Float -> Float -> Fuzz.Fuzzer Float
-}
floatRange : Float -> Float -> Elm.Expression
floatRange floatRangeArg floatRangeArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "floatRange"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.float ])
                    )
            }
        )
        [ Elm.float floatRangeArg, Elm.float floatRangeArg0 ]


{-| Fuzzer generating floats in range `n..Infinity`.

The positive part of the range will shrink nicely, the negative part will shrink uniformly.

The fuzzer will occasionally try the minimum, 0 (if in range) and Infinity.

floatAtLeast: Float -> Fuzz.Fuzzer Float
-}
floatAtLeast : Float -> Elm.Expression
floatAtLeast floatAtLeastArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "floatAtLeast"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.float ])
                    )
            }
        )
        [ Elm.float floatAtLeastArg ]


{-| Fuzzer generating floats in range `-Infinity..n`.

The negative part of the range will shrink nicely, the positive part will shrink uniformly.

The fuzzer will occasionally try the maximum, 0 (if in range) and -Infinity.

floatAtMost: Float -> Fuzz.Fuzzer Float
-}
floatAtMost : Float -> Elm.Expression
floatAtMost floatAtMostArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "floatAtMost"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.float ])
                    )
            }
        )
        [ Elm.float floatAtMostArg ]


{-| A fuzzer for arbitrary Unicode char values.

Avoids surrogate pairs or their components (`0xD800..0xDFFF`).

Will prefer ASCII characters, whitespace, and some examples known to cause
trouble, like combining diacritics marks and emojis.

char: Fuzz.Fuzzer Char.Char
-}
char : Elm.Expression
char =
    Elm.value
        { importFrom = [ "Fuzz" ]
        , name = "char"
        , annotation = Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.char ])
        }


{-| A fuzzer for simple ASCII char values (range 32..126).
Skips control characters and the extended character set.

For more serious char fuzzing look at [`Fuzz.char`](#char) which generates the
whole Unicode range.

asciiChar: Fuzz.Fuzzer Char.Char
-}
asciiChar : Elm.Expression
asciiChar =
    Elm.value
        { importFrom = [ "Fuzz" ]
        , name = "asciiChar"
        , annotation = Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.char ])
        }


{-| Generates random unicode strings of up to 10 characters.

string: Fuzz.Fuzzer String
-}
string : Elm.Expression
string =
    Elm.value
        { importFrom = [ "Fuzz" ]
        , name = "string"
        , annotation = Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.string ])
        }


{-| Generates random unicode strings of a given length.

Note that some unicode characters have `String.length` of 2. This fuzzer will
make sure the `String.length` of the returned string is equal to the wanted
length, even if it will mean there are less characters. If you instead want it
to give N characters even if their `String.length` will be above N, you can use

    Fuzz.listOfLength n Fuzz.char
        |> Fuzz.map String.fromList

stringOfLength: Int -> Fuzz.Fuzzer String
-}
stringOfLength : Int -> Elm.Expression
stringOfLength stringOfLengthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "stringOfLength"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.string ])
                    )
            }
        )
        [ Elm.int stringOfLengthArg ]


{-| Generates random unicode strings of length between the given limits.

Note that some unicode characters have `String.length` of 2. This fuzzer will
make sure the `String.length` of the returned string is equal to the wanted
length, even if it will mean there are less characters. If you instead want it
to give between MIN and MAX characters even if their `String.length` will be
above MAX, you can use

    Fuzz.listOfLengthBetween min max Fuzz.char
        |> Fuzz.map String.fromList

stringOfLengthBetween: Int -> Int -> Fuzz.Fuzzer String
-}
stringOfLengthBetween : Int -> Int -> Elm.Expression
stringOfLengthBetween stringOfLengthBetweenArg stringOfLengthBetweenArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "stringOfLengthBetween"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.int ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.string ])
                    )
            }
        )
        [ Elm.int stringOfLengthBetweenArg, Elm.int stringOfLengthBetweenArg0 ]


{-| Generates random ASCII strings of up to 10 characters.

asciiString: Fuzz.Fuzzer String
-}
asciiString : Elm.Expression
asciiString =
    Elm.value
        { importFrom = [ "Fuzz" ]
        , name = "asciiString"
        , annotation = Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.string ])
        }


{-| Generates random ASCII strings of a given length.

asciiStringOfLength: Int -> Fuzz.Fuzzer String
-}
asciiStringOfLength : Int -> Elm.Expression
asciiStringOfLength asciiStringOfLengthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "asciiStringOfLength"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.string ])
                    )
            }
        )
        [ Elm.int asciiStringOfLengthArg ]


{-| Generates random ASCII strings of length between the given limits.

asciiStringOfLengthBetween: Int -> Int -> Fuzz.Fuzzer String
-}
asciiStringOfLengthBetween : Int -> Int -> Elm.Expression
asciiStringOfLengthBetween asciiStringOfLengthBetweenArg asciiStringOfLengthBetweenArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "asciiStringOfLengthBetween"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.int ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.string ])
                    )
            }
        )
        [ Elm.int asciiStringOfLengthBetweenArg
        , Elm.int asciiStringOfLengthBetweenArg0
        ]


{-| Create a fuzzer of pairs from two fuzzers.

pair: Fuzz.Fuzzer a -> Fuzz.Fuzzer b -> Fuzz.Fuzzer ( a, b )
-}
pair : Elm.Expression -> Elm.Expression -> Elm.Expression
pair pairArg pairArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "pair"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.tuple (Type.var "a") (Type.var "b") ]
                        )
                    )
            }
        )
        [ pairArg, pairArg0 ]


{-| Create a fuzzer of triples from three fuzzers.

triple: Fuzz.Fuzzer a -> Fuzz.Fuzzer b -> Fuzz.Fuzzer c -> Fuzz.Fuzzer ( a, b, c )
-}
triple : Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
triple tripleArg tripleArg0 tripleArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "triple"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ]
                        ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.triple
                                (Type.var "a")
                                (Type.var "b")
                                (Type.var "c")
                            ]
                        )
                    )
            }
        )
        [ tripleArg, tripleArg0, tripleArg1 ]


{-| Given a fuzzer of a type, create a fuzzer of a list of that type.
Generates random lists of varying length, up to 32 elements.

list: Fuzz.Fuzzer a -> Fuzz.Fuzzer (List a)
-}
list : Elm.Expression -> Elm.Expression
list listArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "list"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ] ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.list (Type.var "a") ]
                        )
                    )
            }
        )
        [ listArg ]


{-| Given a fuzzer of a type, create a fuzzer of a list of that type.
Generates random lists of exactly the specified length.

listOfLength: Int -> Fuzz.Fuzzer a -> Fuzz.Fuzzer (List a)
-}
listOfLength : Int -> Elm.Expression -> Elm.Expression
listOfLength listOfLengthArg listOfLengthArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "listOfLength"
            , annotation =
                Just
                    (Type.function
                        [ Type.int
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.list (Type.var "a") ]
                        )
                    )
            }
        )
        [ Elm.int listOfLengthArg, listOfLengthArg0 ]


{-| Given a fuzzer of a type, create a fuzzer of a list of that type.
Generates random lists of length between the two given integers.

listOfLengthBetween: Int -> Int -> Fuzz.Fuzzer a -> Fuzz.Fuzzer (List a)
-}
listOfLengthBetween : Int -> Int -> Elm.Expression -> Elm.Expression
listOfLengthBetween listOfLengthBetweenArg listOfLengthBetweenArg0 listOfLengthBetweenArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "listOfLengthBetween"
            , annotation =
                Just
                    (Type.function
                        [ Type.int
                        , Type.int
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.list (Type.var "a") ]
                        )
                    )
            }
        )
        [ Elm.int listOfLengthBetweenArg
        , Elm.int listOfLengthBetweenArg0
        , listOfLengthBetweenArg1
        ]


{-| A fuzzer that shuffles the given list.

shuffledList: List a -> Fuzz.Fuzzer (List a)
-}
shuffledList : List Elm.Expression -> Elm.Expression
shuffledList shuffledListArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "shuffledList"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.var "a") ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.list (Type.var "a") ]
                        )
                    )
            }
        )
        [ Elm.list shuffledListArg ]


{-| Given a fuzzer of a type, create a fuzzer of an array of that type.
Generates random arrays of varying length, favoring shorter arrays.

array: Fuzz.Fuzzer a -> Fuzz.Fuzzer (Array.Array a)
-}
array : Elm.Expression -> Elm.Expression
array arrayArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "array"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ] ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.namedWith
                                [ "Array" ]
                                "Array"
                                [ Type.var "a" ]
                            ]
                        )
                    )
            }
        )
        [ arrayArg ]


{-| Given a fuzzer of a type, create a fuzzer of a maybe for that type.

maybe: Fuzz.Fuzzer a -> Fuzz.Fuzzer (Maybe a)
-}
maybe : Elm.Expression -> Elm.Expression
maybe maybeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "maybe"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ] ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.maybe (Type.var "a") ]
                        )
                    )
            }
        )
        [ maybeArg ]


{-| Given fuzzers for an error type and a success type, create a fuzzer for
a result.

result: 
    Fuzz.Fuzzer error
    -> Fuzz.Fuzzer value
    -> Fuzz.Fuzzer (Result.Result error value)
-}
result : Elm.Expression -> Elm.Expression -> Elm.Expression
result resultArg resultArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "result"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.var "error" ]
                        , Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.var "value" ]
                        ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.namedWith
                                [ "Result" ]
                                "Result"
                                [ Type.var "error", Type.var "value" ]
                            ]
                        )
                    )
            }
        )
        [ resultArg, resultArg0 ]


{-| A fuzzer for boolean values. It's useful when building up fuzzers of complex
types that contain a boolean somewhere.

We recommend against writing tests fuzzing over booleans. Write a unit test for
the true and false cases explicitly.

Simplifies in order `False < True`.

bool: Fuzz.Fuzzer Bool
-}
bool : Elm.Expression
bool =
    Elm.value
        { importFrom = [ "Fuzz" ]
        , name = "bool"
        , annotation = Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.bool ])
        }


{-| A fuzzer for the unit value. Unit is a type with only one value, commonly
used as a placeholder.

unit: Fuzz.Fuzzer ()
-}
unit : Elm.Expression
unit =
    Elm.value
        { importFrom = [ "Fuzz" ]
        , name = "unit"
        , annotation = Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.unit ])
        }


{-| A fuzzer for order values.

Simplifies in order `LT < EQ < GT`.

order: Fuzz.Fuzzer Basics.Order
-}
order : Elm.Expression
order =
    Elm.value
        { importFrom = [ "Fuzz" ]
        , name = "order"
        , annotation =
            Just
                (Type.namedWith
                    [ "Fuzz" ]
                    "Fuzzer"
                    [ Type.namedWith [ "Basics" ] "Order" [] ]
                )
        }


{-| A fuzzer for boolean values, generating True with the given probability
(0.0 = always False, 1.0 = always True).

Probabilities outside the `0..1` range will be clamped to `0..1`.

Simplifies towards False (if not prevented to do that by using probability >= 1).

weightedBool: Float -> Fuzz.Fuzzer Bool
-}
weightedBool : Float -> Elm.Expression
weightedBool weightedBoolArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "weightedBool"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.bool ])
                    )
            }
        )
        [ Elm.float weightedBoolArg ]


{-| Choose one of the given fuzzers at random. Each fuzzer has an equal chance
of being chosen; to customize the probabilities, use [`Fuzz.frequency`](#frequency).

This fuzzer will simplify towards the fuzzers earlier in the list (each of which
will also apply its own way to simplify the values).

    Fuzz.oneOf
        [ Fuzz.intRange 0 3
        , Fuzz.intRange 7 9
        ]

oneOf: List (Fuzz.Fuzzer a) -> Fuzz.Fuzzer a
-}
oneOf : List Elm.Expression -> Elm.Expression
oneOf oneOfArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "oneOf"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                            )
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
        )
        [ Elm.list oneOfArg ]


{-| Choose one of the given values at random. Each value has an equal chance
of being chosen; to customize the probabilities, use
[`Fuzz.frequencyValues`](#frequencyValues).

This fuzzer will simplify towards the values earlier in the list.

    Fuzz.oneOfValues
        [ 999
        , -42
        ]

oneOfValues: List a -> Fuzz.Fuzzer a
-}
oneOfValues : List Elm.Expression -> Elm.Expression
oneOfValues oneOfValuesArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "oneOfValues"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.var "a") ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
        )
        [ Elm.list oneOfValuesArg ]


{-| Create a new `Fuzzer` by providing a list of probabilistic weights to use
with other fuzzers.
For example, to create a `Fuzzer` that has a 1/4 chance of generating an int
between -1 and -100, and a 3/4 chance of generating one between 1 and 100,
you could do this:

    Fuzz.frequency
        [ ( 1, Fuzz.intRange -100 -1 )
        , ( 3, Fuzz.intRange 1 100 )
        ]

This fuzzer will simplify towards the fuzzers earlier in the list (each of which
will also apply its own way to simplify the values).

There are a few circumstances in which this function will return an invalid
fuzzer, which causes it to fail any test that uses it:

  - If you provide an empty list of frequencies
  - If any of the weights are less than 0
  - If the weights sum to 0

Be careful recursively using this fuzzer in its arguments. Often using
[`Fuzz.map`](#map) is a better way to do what you want. If you are fuzzing a
tree-like data structure, you should include a depth limit so to avoid infinite
recursion, like so:

    type Tree
        = Leaf
        | Branch Tree Tree

    tree : Int -> Fuzzer Tree
    tree i =
        if i <= 0 then
            Fuzz.constant Leaf

        else
            Fuzz.frequency
                [ ( 1, Fuzz.constant Leaf )
                , ( 2, Fuzz.map2 Branch (tree (i - 1)) (tree (i - 1)) )
                ]

frequency: List ( Float, Fuzz.Fuzzer a ) -> Fuzz.Fuzzer a
-}
frequency : List Elm.Expression -> Elm.Expression
frequency frequencyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "frequency"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.tuple
                                Type.float
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                )
                            )
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
        )
        [ Elm.list frequencyArg ]


{-| Create a `Fuzzer` by providing a list of probabilistic weights to use with
values.
For example, to create a `Fuzzer` that has a 1/4 chance of generating a string
"foo", and a 3/4 chance of generating a string "bar", you could do this:

    Fuzz.frequencyValues
        [ ( 1, "foo" )
        , ( 3, "bar" )
        ]

This fuzzer will simplify towards the values earlier in the list.

There are a few circumstances in which this function will return an invalid
fuzzer, which causes it to fail any test that uses it:

  - If you provide an empty list of frequencies
  - If any of the weights are less than 0
  - If the weights sum to 0

frequencyValues: List ( Float, a ) -> Fuzz.Fuzzer a
-}
frequencyValues : List Elm.Expression -> Elm.Expression
frequencyValues frequencyValuesArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "frequencyValues"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.tuple Type.float (Type.var "a")) ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
        )
        [ Elm.list frequencyValuesArg ]


{-| Create a fuzzer that only and always returns the value provided, and performs
no simplifying. This is hardly random, and so this function is best used as a
helper when creating more complicated fuzzers.

constant: a -> Fuzz.Fuzzer a
-}
constant : Elm.Expression -> Elm.Expression
constant constantArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "constant"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "a" ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
        )
        [ constantArg ]


{-| A fuzzer that is invalid for the provided reason. Any fuzzers built with it
are also invalid. Any tests using an invalid fuzzer fail.

invalid: String -> Fuzz.Fuzzer a
-}
invalid : String -> Elm.Expression
invalid invalidArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "invalid"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
        )
        [ Elm.string invalidArg ]


{-| A fuzzer that only lets through values satisfying the given predicate
function.

Warning: By using `Fuzz.filter` you can get exceptionally unlucky and get 15
rejections in a row, in which case the test will fluke out and fail!

It's always preferable to get to your wanted values using [`Fuzz.map`](#map),
as you don't run the risk of rejecting too may values and slowing down your
tests, for example using `Fuzz.intRange 0 5 |> Fuzz.map (\x -> x * 2)` instead
of `Fuzz.intRange 0 9 |> Fuzz.filter (\x -> modBy 2 x == 0)`.

If you want to generate indefinitely until you find a satisfactory value (with
a risk of infinite loop depending on the predicate), you can use this pattern:

    goodItemFuzzer =
        itemFuzzer
            |> Fuzz.andThen
                (\item ->
                    if isGood item then
                        Fuzz.constant item

                    else
                        goodItemFuzzer
                )

filter: (a -> Bool) -> Fuzz.Fuzzer a -> Fuzz.Fuzzer a
-}
filter : (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
filter filterArg filterArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "filter"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] Type.bool
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
        )
        [ Elm.functionReduced "filterUnpack" filterArg, filterArg0 ]


{-| Map a function over a fuzzer.

map: (a -> b) -> Fuzz.Fuzzer a -> Fuzz.Fuzzer b
-}
map : (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
map mapArg mapArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] (Type.var "b")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ])
                    )
            }
        )
        [ Elm.functionReduced "mapUnpack" mapArg, mapArg0 ]


{-| Map over two fuzzers.

map2: (a -> b -> c) -> Fuzz.Fuzzer a -> Fuzz.Fuzzer b -> Fuzz.Fuzzer c
-}
map2 :
    (Elm.Expression -> Elm.Expression -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
map2 map2Arg map2Arg0 map2Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map2"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a", Type.var "b" ]
                            (Type.var "c")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ])
                    )
            }
        )
        [ Elm.functionReduced
            "map2Unpack"
            (\functionReducedUnpack ->
                Elm.functionReduced "unpack" (map2Arg functionReducedUnpack)
            )
        , map2Arg0
        , map2Arg1
        ]


{-| Map over three fuzzers.

map3: 
    (a -> b -> c -> d)
    -> Fuzz.Fuzzer a
    -> Fuzz.Fuzzer b
    -> Fuzz.Fuzzer c
    -> Fuzz.Fuzzer d
-}
map3 :
    (Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
map3 map3Arg map3Arg0 map3Arg1 map3Arg2 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map3"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a", Type.var "b", Type.var "c" ]
                            (Type.var "d")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "d" ])
                    )
            }
        )
        [ Elm.functionReduced
            "map3Unpack"
            (\functionReducedUnpack ->
                Elm.functionReduced
                    "unpack"
                    (\functionReducedUnpack0 ->
                        Elm.functionReduced
                            "unpack"
                            (map3Arg functionReducedUnpack
                                functionReducedUnpack0
                            )
                    )
            )
        , map3Arg0
        , map3Arg1
        , map3Arg2
        ]


{-| Map over four fuzzers.

map4: 
    (a -> b -> c -> d -> e)
    -> Fuzz.Fuzzer a
    -> Fuzz.Fuzzer b
    -> Fuzz.Fuzzer c
    -> Fuzz.Fuzzer d
    -> Fuzz.Fuzzer e
-}
map4 :
    (Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
map4 map4Arg map4Arg0 map4Arg1 map4Arg2 map4Arg3 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map4"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a"
                            , Type.var "b"
                            , Type.var "c"
                            , Type.var "d"
                            ]
                            (Type.var "e")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "d" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "e" ])
                    )
            }
        )
        [ Elm.functionReduced
            "map4Unpack"
            (\functionReducedUnpack ->
                Elm.functionReduced
                    "unpack"
                    (\functionReducedUnpack0 ->
                        Elm.functionReduced
                            "unpack"
                            (\functionReducedUnpack_2_1_2_0_2_0_2_0_0 ->
                                Elm.functionReduced
                                    "unpack"
                                    (map4Arg functionReducedUnpack
                                         functionReducedUnpack0
                                        functionReducedUnpack_2_1_2_0_2_0_2_0_0
                                    )
                            )
                    )
            )
        , map4Arg0
        , map4Arg1
        , map4Arg2
        , map4Arg3
        ]


{-| Map over five fuzzers.

map5: 
    (a -> b -> c -> d -> e -> f)
    -> Fuzz.Fuzzer a
    -> Fuzz.Fuzzer b
    -> Fuzz.Fuzzer c
    -> Fuzz.Fuzzer d
    -> Fuzz.Fuzzer e
    -> Fuzz.Fuzzer f
-}
map5 :
    (Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
map5 map5Arg map5Arg0 map5Arg1 map5Arg2 map5Arg3 map5Arg4 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map5"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a"
                            , Type.var "b"
                            , Type.var "c"
                            , Type.var "d"
                            , Type.var "e"
                            ]
                            (Type.var "f")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "d" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "e" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "f" ])
                    )
            }
        )
        [ Elm.functionReduced
            "map5Unpack"
            (\functionReducedUnpack ->
                Elm.functionReduced
                    "unpack"
                    (\functionReducedUnpack0 ->
                        Elm.functionReduced
                            "unpack"
                            (\functionReducedUnpack_2_1_2_0_2_0_2_0_0 ->
                                Elm.functionReduced
                                    "unpack"
                                    (\functionReducedUnpack_2_1_2_1_2_0_2_0_2_0_0 ->
                                        Elm.functionReduced
                                            "unpack"
                                            (map5Arg functionReducedUnpack
                                                 functionReducedUnpack0
                                                 functionReducedUnpack_2_1_2_0_2_0_2_0_0
                                                functionReducedUnpack_2_1_2_1_2_0_2_0_2_0_0
                                            )
                                    )
                            )
                    )
            )
        , map5Arg0
        , map5Arg1
        , map5Arg2
        , map5Arg3
        , map5Arg4
        ]


{-| Map over six fuzzers.

map6: 
    (a -> b -> c -> d -> e -> f -> g)
    -> Fuzz.Fuzzer a
    -> Fuzz.Fuzzer b
    -> Fuzz.Fuzzer c
    -> Fuzz.Fuzzer d
    -> Fuzz.Fuzzer e
    -> Fuzz.Fuzzer f
    -> Fuzz.Fuzzer g
-}
map6 :
    (Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
map6 map6Arg map6Arg0 map6Arg1 map6Arg2 map6Arg3 map6Arg4 map6Arg5 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map6"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a"
                            , Type.var "b"
                            , Type.var "c"
                            , Type.var "d"
                            , Type.var "e"
                            , Type.var "f"
                            ]
                            (Type.var "g")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "d" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "e" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "f" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "g" ])
                    )
            }
        )
        [ Elm.functionReduced
            "map6Unpack"
            (\functionReducedUnpack ->
                Elm.functionReduced
                    "unpack"
                    (\functionReducedUnpack0 ->
                        Elm.functionReduced
                            "unpack"
                            (\functionReducedUnpack_2_1_2_0_2_0_2_0_0 ->
                                Elm.functionReduced
                                    "unpack"
                                    (\functionReducedUnpack_2_1_2_1_2_0_2_0_2_0_0 ->
                                        Elm.functionReduced
                                            "unpack"
                                            (\functionReducedUnpack_2_1_2_1_2_1_2_0_2_0_2_0_0 ->
                                                Elm.functionReduced
                                                    "unpack"
                                                    (map6Arg
                                                         functionReducedUnpack
                                                         functionReducedUnpack0
                                                         functionReducedUnpack_2_1_2_0_2_0_2_0_0
                                                         functionReducedUnpack_2_1_2_1_2_0_2_0_2_0_0
                                                        functionReducedUnpack_2_1_2_1_2_1_2_0_2_0_2_0_0
                                                    )
                                            )
                                    )
                            )
                    )
            )
        , map6Arg0
        , map6Arg1
        , map6Arg2
        , map6Arg3
        , map6Arg4
        , map6Arg5
        ]


{-| Map over seven fuzzers.

map7: 
    (a -> b -> c -> d -> e -> f -> g -> h)
    -> Fuzz.Fuzzer a
    -> Fuzz.Fuzzer b
    -> Fuzz.Fuzzer c
    -> Fuzz.Fuzzer d
    -> Fuzz.Fuzzer e
    -> Fuzz.Fuzzer f
    -> Fuzz.Fuzzer g
    -> Fuzz.Fuzzer h
-}
map7 :
    (Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
map7 map7Arg map7Arg0 map7Arg1 map7Arg2 map7Arg3 map7Arg4 map7Arg5 map7Arg6 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map7"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a"
                            , Type.var "b"
                            , Type.var "c"
                            , Type.var "d"
                            , Type.var "e"
                            , Type.var "f"
                            , Type.var "g"
                            ]
                            (Type.var "h")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "d" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "e" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "f" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "g" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "h" ])
                    )
            }
        )
        [ Elm.functionReduced
            "map7Unpack"
            (\functionReducedUnpack ->
                Elm.functionReduced
                    "unpack"
                    (\functionReducedUnpack0 ->
                        Elm.functionReduced
                            "unpack"
                            (\functionReducedUnpack_2_1_2_0_2_0_2_0_0 ->
                                Elm.functionReduced
                                    "unpack"
                                    (\functionReducedUnpack_2_1_2_1_2_0_2_0_2_0_0 ->
                                        Elm.functionReduced
                                            "unpack"
                                            (\functionReducedUnpack_2_1_2_1_2_1_2_0_2_0_2_0_0 ->
                                                Elm.functionReduced
                                                    "unpack"
                                                    (\functionReducedUnpack_2_1_2_1_2_1_2_1_2_0_2_0_2_0_0 ->
                                                        Elm.functionReduced
                                                            "unpack"
                                                            (map7Arg
                                                                 functionReducedUnpack
                                                                 functionReducedUnpack0
                                                                 functionReducedUnpack_2_1_2_0_2_0_2_0_0
                                                                 functionReducedUnpack_2_1_2_1_2_0_2_0_2_0_0
                                                                 functionReducedUnpack_2_1_2_1_2_1_2_0_2_0_2_0_0
                                                                functionReducedUnpack_2_1_2_1_2_1_2_1_2_0_2_0_2_0_0
                                                            )
                                                    )
                                            )
                                    )
                            )
                    )
            )
        , map7Arg0
        , map7Arg1
        , map7Arg2
        , map7Arg3
        , map7Arg4
        , map7Arg5
        , map7Arg6
        ]


{-| Map over eight fuzzers.

map8: 
    (a -> b -> c -> d -> e -> f -> g -> h -> i)
    -> Fuzz.Fuzzer a
    -> Fuzz.Fuzzer b
    -> Fuzz.Fuzzer c
    -> Fuzz.Fuzzer d
    -> Fuzz.Fuzzer e
    -> Fuzz.Fuzzer f
    -> Fuzz.Fuzzer g
    -> Fuzz.Fuzzer h
    -> Fuzz.Fuzzer i
-}
map8 :
    (Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
map8 map8Arg map8Arg0 map8Arg1 map8Arg2 map8Arg3 map8Arg4 map8Arg5 map8Arg6 map8Arg7 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map8"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a"
                            , Type.var "b"
                            , Type.var "c"
                            , Type.var "d"
                            , Type.var "e"
                            , Type.var "f"
                            , Type.var "g"
                            , Type.var "h"
                            ]
                            (Type.var "i")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "d" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "e" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "f" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "g" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "h" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "i" ])
                    )
            }
        )
        [ Elm.functionReduced
            "map8Unpack"
            (\functionReducedUnpack ->
                Elm.functionReduced
                    "unpack"
                    (\functionReducedUnpack0 ->
                        Elm.functionReduced
                            "unpack"
                            (\functionReducedUnpack_2_1_2_0_2_0_2_0_0 ->
                                Elm.functionReduced
                                    "unpack"
                                    (\functionReducedUnpack_2_1_2_1_2_0_2_0_2_0_0 ->
                                        Elm.functionReduced
                                            "unpack"
                                            (\functionReducedUnpack_2_1_2_1_2_1_2_0_2_0_2_0_0 ->
                                                Elm.functionReduced
                                                    "unpack"
                                                    (\functionReducedUnpack_2_1_2_1_2_1_2_1_2_0_2_0_2_0_0 ->
                                                        Elm.functionReduced
                                                            "unpack"
                                                            (\functionReducedUnpack_2_1_2_1_2_1_2_1_2_1_2_0_2_0_2_0_0 ->
                                                                Elm.functionReduced
                                                                    "unpack"
                                                                    (map8Arg
                                                                         functionReducedUnpack
                                                                         functionReducedUnpack0
                                                                         functionReducedUnpack_2_1_2_0_2_0_2_0_0
                                                                         functionReducedUnpack_2_1_2_1_2_0_2_0_2_0_0
                                                                         functionReducedUnpack_2_1_2_1_2_1_2_0_2_0_2_0_0
                                                                         functionReducedUnpack_2_1_2_1_2_1_2_1_2_0_2_0_2_0_0
                                                                        functionReducedUnpack_2_1_2_1_2_1_2_1_2_1_2_0_2_0_2_0_0
                                                                    )
                                                            )
                                                    )
                                            )
                                    )
                            )
                    )
            )
        , map8Arg0
        , map8Arg1
        , map8Arg2
        , map8Arg3
        , map8Arg4
        , map8Arg5
        , map8Arg6
        , map8Arg7
        ]


{-| Map over many fuzzers. This can act as `mapN` for `N > 8`.
The argument order is meant to accommodate chaining:

    Fuzz.constant fn
        |> Fuzz.andMap fuzzerA
        |> Fuzz.andMap fuzzerB
        |> Fuzz.andMap fuzzerC

andMap: Fuzz.Fuzzer a -> Fuzz.Fuzzer (a -> b) -> Fuzz.Fuzzer b
-}
andMap : Elm.Expression -> Elm.Expression -> Elm.Expression
andMap andMapArg andMapArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "andMap"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.function [ Type.var "a" ] (Type.var "b") ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ])
                    )
            }
        )
        [ andMapArg, andMapArg0 ]


{-| Use a generated value to decide what fuzzer to use next.

For example, let's say you want to generate a list of given length.
One (not ideal) possible way to do that is first choosing how many elements
will there be (generating a number), `andThen` generating a list with that many
items:

    Fuzz.intRange 1 10
        |> Fuzz.andThen
            (\length ->
                let
                    go : Int -> List a -> Fuzzer (List a)
                    go left acc =
                        if left <= 0 then
                            Fuzz.constant (List.reverse acc)

                        else
                            itemFuzzer
                                |> Fuzz.andThen (\item -> go (length - 1) (item :: acc))
                in
                go length []
            )

This will work! Different fuzzers will have different PRNG usage patterns
though and will shrink with varying success. The currently best known way to
fuzz a list of items is based on a "flip a coin, `andThen` generate a value and
repeat or end" approach, and is implemented in the [`Fuzz.list`](#list) helpers
in this module. Use them instead of rolling your own list generator!

Think of `andThen` as a generalization of [`Fuzz.map`](#map). Inside
[`Fuzz.map`](#map) you don't have the option to fuzz another value based on
what you already have; inside `andThen` you do.

andThen: (a -> Fuzz.Fuzzer b) -> Fuzz.Fuzzer a -> Fuzz.Fuzzer b
-}
andThen : (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
andThen andThenArg andThenArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "andThen"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a" ]
                            (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                            )
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ])
                    )
            }
        )
        [ Elm.functionReduced "andThenUnpack" andThenArg, andThenArg0 ]


{-| A fuzzer that delays its execution. Handy for recursive types and preventing
infinite recursion.

lazy: (() -> Fuzz.Fuzzer a) -> Fuzz.Fuzzer a
-}
lazy : (Elm.Expression -> Elm.Expression) -> Elm.Expression
lazy lazyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "lazy"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.unit ]
                            (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                            )
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
        )
        [ Elm.functionReduced "lazyUnpack" lazyArg ]


{-| Executes every fuzzer in the list and collects their values into the returned
list.

Rejections (eg. from [`Fuzz.filter`](#filter) or [`Fuzz.invalid`](#invalid))
bubble up instead of being discarded.

sequence: List (Fuzz.Fuzzer a) -> Fuzz.Fuzzer (List a)
-}
sequence : List Elm.Expression -> Elm.Expression
sequence sequenceArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "sequence"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.list (Type.var "a") ]
                        )
                    )
            }
        )
        [ Elm.list sequenceArg ]


{-| Runs the Fuzzer-returning function on every item in the list, executes them=
and collects their values into the returned list.

Rejections (eg. from [`Fuzz.filter`](#filter) or [`Fuzz.invalid`](#invalid))
bubble up instead of being discarded.

traverse: (a -> Fuzz.Fuzzer b) -> List a -> Fuzz.Fuzzer (List b)
-}
traverse :
    (Elm.Expression -> Elm.Expression) -> List Elm.Expression -> Elm.Expression
traverse traverseArg traverseArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "traverse"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a" ]
                            (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                            )
                        , Type.list (Type.var "a")
                        ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.list (Type.var "b") ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "traverseUnpack" traverseArg
        , Elm.list traverseArg0
        ]


{-| (Avoid this function if you can! It is only provided as an escape hatch.)

Convert a Random.Generator into a Fuzzer.

Works internally by generating a random seed and running `Random.step`.

Note this will not shrink well (in fact it will shrink randomly, to smaller
_seeds_), as Generators are black boxes from the perspective of Fuzzers. If you
want meaningful shrinking, define fuzzers using the other functions in this
module!

fromGenerator: Random.Generator a -> Fuzz.Fuzzer a
-}
fromGenerator : Elm.Expression -> Elm.Expression
fromGenerator fromGeneratorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "fromGenerator"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Random" ]
                            "Generator"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
        )
        [ fromGeneratorArg ]


annotation_ : { fuzzer : Type.Annotation -> Type.Annotation }
annotation_ =
    { fuzzer =
        \fuzzerArg0 ->
            Type.alias
                moduleName_
                "Fuzzer"
                [ fuzzerArg0 ]
                (Type.namedWith [ "Fuzz", "Internal" ] "Fuzzer" [ Type.var "a" ]
                )
    }


call_ :
    { examples : Elm.Expression -> Elm.Expression -> Elm.Expression
    , labelExamples :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , intRange : Elm.Expression -> Elm.Expression -> Elm.Expression
    , uniformInt : Elm.Expression -> Elm.Expression
    , intAtLeast : Elm.Expression -> Elm.Expression
    , intAtMost : Elm.Expression -> Elm.Expression
    , floatRange : Elm.Expression -> Elm.Expression -> Elm.Expression
    , floatAtLeast : Elm.Expression -> Elm.Expression
    , floatAtMost : Elm.Expression -> Elm.Expression
    , stringOfLength : Elm.Expression -> Elm.Expression
    , stringOfLengthBetween : Elm.Expression -> Elm.Expression -> Elm.Expression
    , asciiStringOfLength : Elm.Expression -> Elm.Expression
    , asciiStringOfLengthBetween :
        Elm.Expression -> Elm.Expression -> Elm.Expression
    , pair : Elm.Expression -> Elm.Expression -> Elm.Expression
    , triple :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , list : Elm.Expression -> Elm.Expression
    , listOfLength : Elm.Expression -> Elm.Expression -> Elm.Expression
    , listOfLengthBetween :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , shuffledList : Elm.Expression -> Elm.Expression
    , array : Elm.Expression -> Elm.Expression
    , maybe : Elm.Expression -> Elm.Expression
    , result : Elm.Expression -> Elm.Expression -> Elm.Expression
    , weightedBool : Elm.Expression -> Elm.Expression
    , oneOf : Elm.Expression -> Elm.Expression
    , oneOfValues : Elm.Expression -> Elm.Expression
    , frequency : Elm.Expression -> Elm.Expression
    , frequencyValues : Elm.Expression -> Elm.Expression
    , constant : Elm.Expression -> Elm.Expression
    , invalid : Elm.Expression -> Elm.Expression
    , filter : Elm.Expression -> Elm.Expression -> Elm.Expression
    , map : Elm.Expression -> Elm.Expression -> Elm.Expression
    , map2 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , map3 :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , map4 :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , map5 :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , map6 :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , map7 :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , map8 :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , andMap : Elm.Expression -> Elm.Expression -> Elm.Expression
    , andThen : Elm.Expression -> Elm.Expression -> Elm.Expression
    , lazy : Elm.Expression -> Elm.Expression
    , sequence : Elm.Expression -> Elm.Expression
    , traverse : Elm.Expression -> Elm.Expression -> Elm.Expression
    , fromGenerator : Elm.Expression -> Elm.Expression
    }
call_ =
    { examples =
        \examplesArg examplesArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "examples"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                ]
                                (Type.list (Type.var "a"))
                            )
                    }
                )
                [ examplesArg, examplesArg0 ]
    , labelExamples =
        \labelExamplesArg labelExamplesArg0 labelExamplesArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "labelExamples"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int
                                , Type.list
                                    (Type.tuple
                                        Type.string
                                        (Type.function
                                            [ Type.var "a" ]
                                            Type.bool
                                        )
                                    )
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                ]
                                (Type.list
                                    (Type.tuple
                                        (Type.list Type.string)
                                        (Type.maybe (Type.var "a"))
                                    )
                                )
                            )
                    }
                )
                [ labelExamplesArg, labelExamplesArg0, labelExamplesArg1 ]
    , intRange =
        \intRangeArg intRangeArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "intRange"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int, Type.int ]
                                (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.int ]
                                )
                            )
                    }
                )
                [ intRangeArg, intRangeArg0 ]
    , uniformInt =
        \uniformIntArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "uniformInt"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.int ]
                                )
                            )
                    }
                )
                [ uniformIntArg ]
    , intAtLeast =
        \intAtLeastArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "intAtLeast"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.int ]
                                )
                            )
                    }
                )
                [ intAtLeastArg ]
    , intAtMost =
        \intAtMostArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "intAtMost"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.int ]
                                )
                            )
                    }
                )
                [ intAtMostArg ]
    , floatRange =
        \floatRangeArg floatRangeArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "floatRange"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.float ]
                                )
                            )
                    }
                )
                [ floatRangeArg, floatRangeArg0 ]
    , floatAtLeast =
        \floatAtLeastArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "floatAtLeast"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.float ]
                                )
                            )
                    }
                )
                [ floatAtLeastArg ]
    , floatAtMost =
        \floatAtMostArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "floatAtMost"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.float ]
                                )
                            )
                    }
                )
                [ floatAtMostArg ]
    , stringOfLength =
        \stringOfLengthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "stringOfLength"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.string ]
                                )
                            )
                    }
                )
                [ stringOfLengthArg ]
    , stringOfLengthBetween =
        \stringOfLengthBetweenArg stringOfLengthBetweenArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "stringOfLengthBetween"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int, Type.int ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.string ]
                                )
                            )
                    }
                )
                [ stringOfLengthBetweenArg, stringOfLengthBetweenArg0 ]
    , asciiStringOfLength =
        \asciiStringOfLengthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "asciiStringOfLength"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.string ]
                                )
                            )
                    }
                )
                [ asciiStringOfLengthArg ]
    , asciiStringOfLengthBetween =
        \asciiStringOfLengthBetweenArg asciiStringOfLengthBetweenArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "asciiStringOfLengthBetween"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int, Type.int ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.string ]
                                )
                            )
                    }
                )
                [ asciiStringOfLengthBetweenArg
                , asciiStringOfLengthBetweenArg0
                ]
    , pair =
        \pairArg pairArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "pair"
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
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.tuple (Type.var "a") (Type.var "b") ]
                                )
                            )
                    }
                )
                [ pairArg, pairArg0 ]
    , triple =
        \tripleArg tripleArg0 tripleArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "triple"
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
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.triple
                                        (Type.var "a")
                                        (Type.var "b")
                                        (Type.var "c")
                                    ]
                                )
                            )
                    }
                )
                [ tripleArg, tripleArg0, tripleArg1 ]
    , list =
        \listArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "list"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.list (Type.var "a") ]
                                )
                            )
                    }
                )
                [ listArg ]
    , listOfLength =
        \listOfLengthArg listOfLengthArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "listOfLength"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.list (Type.var "a") ]
                                )
                            )
                    }
                )
                [ listOfLengthArg, listOfLengthArg0 ]
    , listOfLengthBetween =
        \listOfLengthBetweenArg listOfLengthBetweenArg0 listOfLengthBetweenArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "listOfLengthBetween"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int
                                , Type.int
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.list (Type.var "a") ]
                                )
                            )
                    }
                )
                [ listOfLengthBetweenArg
                , listOfLengthBetweenArg0
                , listOfLengthBetweenArg1
                ]
    , shuffledList =
        \shuffledListArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "shuffledList"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list (Type.var "a") ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.list (Type.var "a") ]
                                )
                            )
                    }
                )
                [ shuffledListArg ]
    , array =
        \arrayArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "array"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.namedWith
                                        [ "Array" ]
                                        "Array"
                                        [ Type.var "a" ]
                                    ]
                                )
                            )
                    }
                )
                [ arrayArg ]
    , maybe =
        \maybeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "maybe"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.maybe (Type.var "a") ]
                                )
                            )
                    }
                )
                [ maybeArg ]
    , result =
        \resultArg resultArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "result"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "error" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "value" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.namedWith
                                        [ "Result" ]
                                        "Result"
                                        [ Type.var "error", Type.var "value" ]
                                    ]
                                )
                            )
                    }
                )
                [ resultArg, resultArg0 ]
    , weightedBool =
        \weightedBoolArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "weightedBool"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.bool ]
                                )
                            )
                    }
                )
                [ weightedBoolArg ]
    , oneOf =
        \oneOfArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "oneOf"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Fuzz" ]
                                        "Fuzzer"
                                        [ Type.var "a" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ oneOfArg ]
    , oneOfValues =
        \oneOfValuesArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "oneOfValues"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list (Type.var "a") ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ oneOfValuesArg ]
    , frequency =
        \frequencyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "frequency"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.tuple
                                        Type.float
                                        (Type.namedWith
                                            [ "Fuzz" ]
                                            "Fuzzer"
                                            [ Type.var "a" ]
                                        )
                                    )
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ frequencyArg ]
    , frequencyValues =
        \frequencyValuesArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "frequencyValues"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.tuple Type.float (Type.var "a"))
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ frequencyValuesArg ]
    , constant =
        \constantArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "constant"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "a" ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ constantArg ]
    , invalid =
        \invalidArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "invalid"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ invalidArg ]
    , filter =
        \filterArg filterArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "filter"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.var "a" ] Type.bool
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ filterArg, filterArg0 ]
    , map =
        \mapArg mapArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "map"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.var "a" ] (Type.var "b")
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "b" ]
                                )
                            )
                    }
                )
                [ mapArg, mapArg0 ]
    , map2 =
        \map2Arg map2Arg0 map2Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "map2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a", Type.var "b" ]
                                    (Type.var "c")
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "b" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "c" ]
                                )
                            )
                    }
                )
                [ map2Arg, map2Arg0, map2Arg1 ]
    , map3 =
        \map3Arg map3Arg0 map3Arg1 map3Arg2 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "map3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a", Type.var "b", Type.var "c" ]
                                    (Type.var "d")
                                , Type.namedWith
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
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "d" ]
                                )
                            )
                    }
                )
                [ map3Arg, map3Arg0, map3Arg1, map3Arg2 ]
    , map4 =
        \map4Arg map4Arg0 map4Arg1 map4Arg2 map4Arg3 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "map4"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a"
                                    , Type.var "b"
                                    , Type.var "c"
                                    , Type.var "d"
                                    ]
                                    (Type.var "e")
                                , Type.namedWith
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
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "d" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "e" ]
                                )
                            )
                    }
                )
                [ map4Arg, map4Arg0, map4Arg1, map4Arg2, map4Arg3 ]
    , map5 =
        \map5Arg map5Arg0 map5Arg1 map5Arg2 map5Arg3 map5Arg4 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "map5"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a"
                                    , Type.var "b"
                                    , Type.var "c"
                                    , Type.var "d"
                                    , Type.var "e"
                                    ]
                                    (Type.var "f")
                                , Type.namedWith
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
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "d" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "e" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "f" ]
                                )
                            )
                    }
                )
                [ map5Arg, map5Arg0, map5Arg1, map5Arg2, map5Arg3, map5Arg4 ]
    , map6 =
        \map6Arg map6Arg0 map6Arg1 map6Arg2 map6Arg3 map6Arg4 map6Arg5 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "map6"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a"
                                    , Type.var "b"
                                    , Type.var "c"
                                    , Type.var "d"
                                    , Type.var "e"
                                    , Type.var "f"
                                    ]
                                    (Type.var "g")
                                , Type.namedWith
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
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "d" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "e" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "f" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "g" ]
                                )
                            )
                    }
                )
                [ map6Arg
                , map6Arg0
                , map6Arg1
                , map6Arg2
                , map6Arg3
                , map6Arg4
                , map6Arg5
                ]
    , map7 =
        \map7Arg map7Arg0 map7Arg1 map7Arg2 map7Arg3 map7Arg4 map7Arg5 map7Arg6 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "map7"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a"
                                    , Type.var "b"
                                    , Type.var "c"
                                    , Type.var "d"
                                    , Type.var "e"
                                    , Type.var "f"
                                    , Type.var "g"
                                    ]
                                    (Type.var "h")
                                , Type.namedWith
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
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "d" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "e" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "f" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "g" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "h" ]
                                )
                            )
                    }
                )
                [ map7Arg
                , map7Arg0
                , map7Arg1
                , map7Arg2
                , map7Arg3
                , map7Arg4
                , map7Arg5
                , map7Arg6
                ]
    , map8 =
        \map8Arg map8Arg0 map8Arg1 map8Arg2 map8Arg3 map8Arg4 map8Arg5 map8Arg6 map8Arg7 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "map8"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a"
                                    , Type.var "b"
                                    , Type.var "c"
                                    , Type.var "d"
                                    , Type.var "e"
                                    , Type.var "f"
                                    , Type.var "g"
                                    , Type.var "h"
                                    ]
                                    (Type.var "i")
                                , Type.namedWith
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
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "d" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "e" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "f" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "g" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "h" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "i" ]
                                )
                            )
                    }
                )
                [ map8Arg
                , map8Arg0
                , map8Arg1
                , map8Arg2
                , map8Arg3
                , map8Arg4
                , map8Arg5
                , map8Arg6
                , map8Arg7
                ]
    , andMap =
        \andMapArg andMapArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "andMap"
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
                                    [ Type.function
                                        [ Type.var "a" ]
                                        (Type.var "b")
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "b" ]
                                )
                            )
                    }
                )
                [ andMapArg, andMapArg0 ]
    , andThen =
        \andThenArg andThenArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "andThen"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a" ]
                                    (Type.namedWith
                                        [ "Fuzz" ]
                                        "Fuzzer"
                                        [ Type.var "b" ]
                                    )
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "b" ]
                                )
                            )
                    }
                )
                [ andThenArg, andThenArg0 ]
    , lazy =
        \lazyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "lazy"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.unit ]
                                    (Type.namedWith
                                        [ "Fuzz" ]
                                        "Fuzzer"
                                        [ Type.var "a" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ lazyArg ]
    , sequence =
        \sequenceArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "sequence"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Fuzz" ]
                                        "Fuzzer"
                                        [ Type.var "a" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.list (Type.var "a") ]
                                )
                            )
                    }
                )
                [ sequenceArg ]
    , traverse =
        \traverseArg traverseArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "traverse"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a" ]
                                    (Type.namedWith
                                        [ "Fuzz" ]
                                        "Fuzzer"
                                        [ Type.var "b" ]
                                    )
                                , Type.list (Type.var "a")
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.list (Type.var "b") ]
                                )
                            )
                    }
                )
                [ traverseArg, traverseArg0 ]
    , fromGenerator =
        \fromGeneratorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "fromGenerator"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Random" ]
                                    "Generator"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ fromGeneratorArg ]
    }


values_ :
    { examples : Elm.Expression
    , labelExamples : Elm.Expression
    , int : Elm.Expression
    , intRange : Elm.Expression
    , uniformInt : Elm.Expression
    , intAtLeast : Elm.Expression
    , intAtMost : Elm.Expression
    , float : Elm.Expression
    , niceFloat : Elm.Expression
    , percentage : Elm.Expression
    , floatRange : Elm.Expression
    , floatAtLeast : Elm.Expression
    , floatAtMost : Elm.Expression
    , char : Elm.Expression
    , asciiChar : Elm.Expression
    , string : Elm.Expression
    , stringOfLength : Elm.Expression
    , stringOfLengthBetween : Elm.Expression
    , asciiString : Elm.Expression
    , asciiStringOfLength : Elm.Expression
    , asciiStringOfLengthBetween : Elm.Expression
    , pair : Elm.Expression
    , triple : Elm.Expression
    , list : Elm.Expression
    , listOfLength : Elm.Expression
    , listOfLengthBetween : Elm.Expression
    , shuffledList : Elm.Expression
    , array : Elm.Expression
    , maybe : Elm.Expression
    , result : Elm.Expression
    , bool : Elm.Expression
    , unit : Elm.Expression
    , order : Elm.Expression
    , weightedBool : Elm.Expression
    , oneOf : Elm.Expression
    , oneOfValues : Elm.Expression
    , frequency : Elm.Expression
    , frequencyValues : Elm.Expression
    , constant : Elm.Expression
    , invalid : Elm.Expression
    , filter : Elm.Expression
    , map : Elm.Expression
    , map2 : Elm.Expression
    , map3 : Elm.Expression
    , map4 : Elm.Expression
    , map5 : Elm.Expression
    , map6 : Elm.Expression
    , map7 : Elm.Expression
    , map8 : Elm.Expression
    , andMap : Elm.Expression
    , andThen : Elm.Expression
    , lazy : Elm.Expression
    , sequence : Elm.Expression
    , traverse : Elm.Expression
    , fromGenerator : Elm.Expression
    }
values_ =
    { examples =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "examples"
            , annotation =
                Just
                    (Type.function
                        [ Type.int
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        ]
                        (Type.list (Type.var "a"))
                    )
            }
    , labelExamples =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "labelExamples"
            , annotation =
                Just
                    (Type.function
                        [ Type.int
                        , Type.list
                            (Type.tuple
                                Type.string
                                (Type.function [ Type.var "a" ] Type.bool)
                            )
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        ]
                        (Type.list
                            (Type.tuple
                                (Type.list Type.string)
                                (Type.maybe (Type.var "a"))
                            )
                        )
                    )
            }
    , int =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "int"
            , annotation =
                Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.int ])
            }
    , intRange =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "intRange"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.int ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.int ])
                    )
            }
    , uniformInt =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "uniformInt"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.int ])
                    )
            }
    , intAtLeast =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "intAtLeast"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.int ])
                    )
            }
    , intAtMost =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "intAtMost"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.int ])
                    )
            }
    , float =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "float"
            , annotation =
                Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.float ])
            }
    , niceFloat =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "niceFloat"
            , annotation =
                Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.float ])
            }
    , percentage =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "percentage"
            , annotation =
                Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.float ])
            }
    , floatRange =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "floatRange"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.float ])
                    )
            }
    , floatAtLeast =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "floatAtLeast"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.float ])
                    )
            }
    , floatAtMost =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "floatAtMost"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.float ])
                    )
            }
    , char =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "char"
            , annotation =
                Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.char ])
            }
    , asciiChar =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "asciiChar"
            , annotation =
                Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.char ])
            }
    , string =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "string"
            , annotation =
                Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.string ])
            }
    , stringOfLength =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "stringOfLength"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.string ])
                    )
            }
    , stringOfLengthBetween =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "stringOfLengthBetween"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.int ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.string ])
                    )
            }
    , asciiString =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "asciiString"
            , annotation =
                Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.string ])
            }
    , asciiStringOfLength =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "asciiStringOfLength"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.string ])
                    )
            }
    , asciiStringOfLengthBetween =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "asciiStringOfLengthBetween"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.int ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.string ])
                    )
            }
    , pair =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "pair"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.tuple (Type.var "a") (Type.var "b") ]
                        )
                    )
            }
    , triple =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "triple"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ]
                        ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.triple
                                (Type.var "a")
                                (Type.var "b")
                                (Type.var "c")
                            ]
                        )
                    )
            }
    , list =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "list"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ] ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.list (Type.var "a") ]
                        )
                    )
            }
    , listOfLength =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "listOfLength"
            , annotation =
                Just
                    (Type.function
                        [ Type.int
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.list (Type.var "a") ]
                        )
                    )
            }
    , listOfLengthBetween =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "listOfLengthBetween"
            , annotation =
                Just
                    (Type.function
                        [ Type.int
                        , Type.int
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.list (Type.var "a") ]
                        )
                    )
            }
    , shuffledList =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "shuffledList"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.var "a") ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.list (Type.var "a") ]
                        )
                    )
            }
    , array =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "array"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ] ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.namedWith
                                [ "Array" ]
                                "Array"
                                [ Type.var "a" ]
                            ]
                        )
                    )
            }
    , maybe =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "maybe"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ] ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.maybe (Type.var "a") ]
                        )
                    )
            }
    , result =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "result"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.var "error" ]
                        , Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.var "value" ]
                        ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.namedWith
                                [ "Result" ]
                                "Result"
                                [ Type.var "error", Type.var "value" ]
                            ]
                        )
                    )
            }
    , bool =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "bool"
            , annotation =
                Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.bool ])
            }
    , unit =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "unit"
            , annotation =
                Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.unit ])
            }
    , order =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "order"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Fuzz" ]
                        "Fuzzer"
                        [ Type.namedWith [ "Basics" ] "Order" [] ]
                    )
            }
    , weightedBool =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "weightedBool"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.bool ])
                    )
            }
    , oneOf =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "oneOf"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                            )
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
    , oneOfValues =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "oneOfValues"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.var "a") ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
    , frequency =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "frequency"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.tuple
                                Type.float
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                )
                            )
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
    , frequencyValues =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "frequencyValues"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.tuple Type.float (Type.var "a")) ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
    , constant =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "constant"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "a" ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
    , invalid =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "invalid"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
    , filter =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "filter"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] Type.bool
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
    , map =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] (Type.var "b")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ])
                    )
            }
    , map2 =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map2"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a", Type.var "b" ]
                            (Type.var "c")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ])
                    )
            }
    , map3 =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map3"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a", Type.var "b", Type.var "c" ]
                            (Type.var "d")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "d" ])
                    )
            }
    , map4 =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map4"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a"
                            , Type.var "b"
                            , Type.var "c"
                            , Type.var "d"
                            ]
                            (Type.var "e")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "d" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "e" ])
                    )
            }
    , map5 =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map5"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a"
                            , Type.var "b"
                            , Type.var "c"
                            , Type.var "d"
                            , Type.var "e"
                            ]
                            (Type.var "f")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "d" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "e" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "f" ])
                    )
            }
    , map6 =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map6"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a"
                            , Type.var "b"
                            , Type.var "c"
                            , Type.var "d"
                            , Type.var "e"
                            , Type.var "f"
                            ]
                            (Type.var "g")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "d" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "e" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "f" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "g" ])
                    )
            }
    , map7 =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map7"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a"
                            , Type.var "b"
                            , Type.var "c"
                            , Type.var "d"
                            , Type.var "e"
                            , Type.var "f"
                            , Type.var "g"
                            ]
                            (Type.var "h")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "d" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "e" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "f" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "g" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "h" ])
                    )
            }
    , map8 =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map8"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a"
                            , Type.var "b"
                            , Type.var "c"
                            , Type.var "d"
                            , Type.var "e"
                            , Type.var "f"
                            , Type.var "g"
                            , Type.var "h"
                            ]
                            (Type.var "i")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "d" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "e" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "f" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "g" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "h" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "i" ])
                    )
            }
    , andMap =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "andMap"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.function [ Type.var "a" ] (Type.var "b") ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ])
                    )
            }
    , andThen =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "andThen"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a" ]
                            (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                            )
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ])
                    )
            }
    , lazy =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "lazy"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.unit ]
                            (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                            )
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
    , sequence =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "sequence"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.list (Type.var "a") ]
                        )
                    )
            }
    , traverse =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "traverse"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a" ]
                            (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                            )
                        , Type.list (Type.var "a")
                        ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.list (Type.var "b") ]
                        )
                    )
            }
    , fromGenerator =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "fromGenerator"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Random" ]
                            "Generator"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
    }


