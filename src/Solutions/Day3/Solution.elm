module Solutions.Day3.Solution exposing (..)

{-| 
-}


import Solutions.Day3.Input as Input
import List.Extra exposing (Step(..))

type alias Item = Char
type alias Priority = Int
type alias CompartmentItems = String
type alias Rucksack = String


itemIsInCompartment : CompartmentItems -> Item -> Maybe Priority
itemIsInCompartment items item =
    if List.member item (String.toList items) then
        Just (prioritiseItem item)
    else
        Nothing

itemIsInRucksack : Rucksack -> Item -> Maybe Item
itemIsInRucksack items item =
    if List.member item (String.toList items) then
        Just item
    else
        Nothing

processCompartments : (CompartmentItems, CompartmentItems) -> Maybe Priority
processCompartments (compartment1, compartment2) =
    String.toList compartment1
        |> List.Extra.stoppableFoldl
            (\item acc ->
                case acc of
                    Just _ ->
                        Stop acc
                    Nothing ->
                        Continue (itemIsInCompartment compartment2 item)
            )
            Nothing

splitItemsByCompartment : Rucksack -> (CompartmentItems, CompartmentItems)
splitItemsByCompartment rucksack =
    let
        numItems =
            (String.length rucksack) // 2
    in
    (String.dropRight numItems rucksack
    , String.dropLeft numItems rucksack)

prioritiseItem : Item -> Priority
prioritiseItem item =
    if Char.isUpper item then
        (Char.toCode item) - 38
    else
        (Char.toCode item) - 96


-- part B

splitIntoGroupsOfThree : List Rucksack -> List (Rucksack, Rucksack, Rucksack)
splitIntoGroupsOfThree allRucksacks =
    List.Extra.greedyGroupsOf 3 allRucksacks
        |> List.filterMap (\group ->
            case group of
                [r1, r2, r3] ->
                    Just (r1, r2, r3)
                _ ->
                    Nothing
            )


findCommonItem : (Rucksack, Rucksack, Rucksack) -> Maybe Item
findCommonItem (r1, r2, r3) =
    List.Extra.stoppableFoldl
        (\item acc ->
            case acc of
                Just common ->
                    case (itemIsInRucksack r3 common) of
                        Just commonAllThree ->
                            Stop (Just commonAllThree)
                        Nothing ->
                            Continue (itemIsInRucksack r2 item)
                Nothing ->
                    Continue (itemIsInRucksack r2 item)
        )
        Nothing
        (String.toList r1)

---------

solveA : String -> String
solveA input =
    String.lines input
        |> List.map splitItemsByCompartment
        |> List.filterMap processCompartments
        |> List.sum
        |> String.fromInt
    




solveB : String -> String
solveB input =
    String.lines input
        |> splitIntoGroupsOfThree
        |> List.filterMap findCommonItem
        |> List.map prioritiseItem
        |> List.sum
        |> String.fromInt


partA : String -> String
partA inputType =
    case inputType of
        "test" ->
            solveA Input.testInput

        "input" ->
            solveA Input.input

        _ ->
            "supported args are \"test\" or \"input\""


partB : String -> String
partB inputType =
    case inputType of
        "test" ->
            solveB Input.testInput

        "input" ->
            solveB Input.input

        _ ->
            "supported args are \"test\" or \"input\""


