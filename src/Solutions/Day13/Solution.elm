module Solutions.Day13.Solution exposing (..)

{-| 
-}


import Solutions.Day13.Input as Input
import Parser exposing (Parser, succeed, (|=), lazy, oneOf, int, spaces, sequence, Trailing(..))
import Utils
import List.Extra

type Packet = PacketList (List Packet) 
    | PacketSingle Int

type alias Packets = List Packet

type alias PacketsPair = (Packets, Packets)

tieBreakerFn : List a -> List a -> Order
tieBreakerFn left right =
    if (List.length left) < (List.length right) then
        LT
    else if (List.length left) > (List.length right) then
        GT
    else
        EQ

comparePacket : Packet -> Packet -> Order
comparePacket left right =
    case (left, right) of
        (PacketSingle leftInt, PacketSingle rightInt) ->
            compare leftInt rightInt

        (PacketSingle _, PacketList _) ->
            comparePacket (PacketList [left]) right

        (PacketList _, PacketSingle _) ->
            comparePacket left (PacketList [right])

        (PacketList leftList, PacketList rightList) ->
            let
                listComparison =
                    List.Extra.zip leftList rightList
                        |> List.Extra.stoppableFoldl (\(l, r) _ ->
                            case comparePacket l r of
                                LT ->
                                    List.Extra.Stop LT
                                GT ->
                                    List.Extra.Stop GT
                                EQ ->
                                    List.Extra.Continue EQ
                        ) EQ

            in
            case listComparison of
                EQ ->
                    tieBreakerFn leftList rightList
                _ ->
                    listComparison


packetsComparison : PacketsPair -> Order
packetsComparison (left, right) =
    comparePacket (PacketList left) (PacketList right)

packetsComparisonPt2 : Packets -> Packets -> Order
packetsComparisonPt2 left right =
    comparePacket (PacketList left) (PacketList right)


getIndexSum : List PacketsPair -> Int
getIndexSum allPacketPairs =
    List.indexedMap (\i pair ->
        case packetsComparison pair of
            GT ->
                0
            _ ->
                i + 1
        )
        allPacketPairs
            |> List.sum
    


parsePacket : Parser Packet
parsePacket =
    oneOf
        [ succeed PacketList
            |= lazy (\_ -> parseListOfPacket)
        , succeed PacketSingle
            |= lazy (\_ -> int)
        ]


parseListOfPacket : Parser (List Packet)
parseListOfPacket =
    sequence
        { start = "["
        , separator = ","
        , end = "]"
        , spaces = spaces
        , item = parsePacket
        , trailing = Forbidden }

parseLine : String -> Maybe Packets
parseLine line =
    Parser.run parseListOfPacket line
        |> Result.toMaybe

parseLinePair : List String -> Maybe PacketsPair
parseLinePair lines =
    case lines of
        first :: second :: [] ->
            Maybe.map2 (\firstPackets secondPackets ->
                (firstPackets, secondPackets)
            )
            (parseLine first)
            (parseLine second)
        _ ->
            Nothing


parseInput : String -> List PacketsPair
parseInput input =
    String.lines input
        |> Utils.splitOnEmptyLine
        |> List.filterMap parseLinePair


solveA : String -> String
solveA input =
    parseInput input
        |> getIndexSum
        |> String.fromInt


solveB : String -> String
solveB input =
    let
        dividerPackets2 =
            Maybe.withDefault [] (parseLine "[[2]]")

        dividerPackets6 =
            Maybe.withDefault [] (parseLine "[[6]]")

        sortedPackets =
            String.lines input
                |> Utils.splitOnEmptyLine
                |> List.concat
                |> List.filterMap parseLine
                |> (++) [dividerPackets2, dividerPackets6]
                |> List.sortWith packetsComparisonPt2

        indexOfFirstDivider =
            Maybe.withDefault (-1) (List.Extra.findIndex ((==) dividerPackets2) sortedPackets)

        indexOfSecondDivider =
            Maybe.withDefault (-1) (List.Extra.findIndex ((==) dividerPackets6) sortedPackets)

    in
    (indexOfFirstDivider + 1) * (indexOfSecondDivider + 1)
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


