module Solutions.Day19.Solution exposing (..)

{-| 
-}


import Solutions.Day19.Input as Input
import Parser exposing (Parser, succeed, spaces, int, symbol, keyword, (|.), (|=))

type Mineral = Ore
    | Clay
    | Obsidian
    | Geode

type alias MineralMap =
    { ore : Int
    , clay : Int
    , obsidian : Int
    , geode : Int
    }

emptyMineralMap : MineralMap
emptyMineralMap =
    { ore = 0
    , clay = 0 
    , obsidian = 0
    , geode = 0
    }

type alias BluePrint =
    { index : Int
    , oreCost : MineralMap
    , clayCost : MineralMap
    , obsidianCost : MineralMap
    , geodeCost : MineralMap
    }

type alias State =
    { minute : Int
    , blueprint : BluePrint
    , collected : MineralMap
    , robots : MineralMap
    }

constructBluePrint : Int -> MineralMap -> MineralMap -> MineralMap -> MineralMap -> BluePrint
constructBluePrint index oreCost clayCost obsidianCost geodeCost =
    { index = index
    , oreCost = oreCost
    , clayCost = clayCost
    , obsidianCost = obsidianCost
    , geodeCost = geodeCost
    }


parseGeodeCost : Parser MineralMap
parseGeodeCost =
    succeed (\ore obsidian -> { emptyMineralMap | ore = ore, obsidian = obsidian })
        |. keyword "Each geode robot costs"
        |. spaces
        |= int
        |. spaces
        |. keyword "ore and"
        |. spaces
        |= int
        |. spaces
        |. keyword "obsidian."

parseObsideanCost : Parser MineralMap
parseObsideanCost =
    succeed (\ore clay -> { emptyMineralMap | ore = ore, clay = clay })
        |. keyword "Each obsidian robot costs"
        |. spaces
        |= int
        |. spaces
        |. keyword "ore and"
        |. spaces
        |= int
        |. spaces
        |. keyword "clay."

parseClayCost : Parser MineralMap
parseClayCost =
    succeed (\n -> { emptyMineralMap | ore = n })
        |. keyword "Each clay robot costs"
        |. spaces
        |= int
        |. spaces
        |. keyword "ore."

parseOreCost : Parser MineralMap
parseOreCost =
    succeed (\n -> { emptyMineralMap | ore = n })
        |. keyword "Each ore robot costs"
        |. spaces
        |= int
        |. spaces
        |. keyword "ore."

parseBlueprint : Parser BluePrint
parseBlueprint =
    succeed constructBluePrint
        |. keyword "Blueprint"
        |. spaces
        |= int
        |. symbol ":"
        |. spaces
        |= parseOreCost
        |. spaces
        |= parseClayCost
        |. spaces
        |= parseObsideanCost
        |. spaces
        |= parseGeodeCost

initialiseState : BluePrint -> State
initialiseState blueprint =
    { blueprint = blueprint
    , minute = 1
    , collected = emptyMineralMap
    , robots = { emptyMineralMap | ore = 1 }
    }


canBuyAGeodeRobot : BluePrint -> MineralMap -> Bool
canBuyAGeodeRobot { geodeCost } { obsidian, ore } =
    ore >= geodeCost.ore && obsidian >= geodeCost.obsidian


canBuyAnObsidianRobot : BluePrint -> MineralMap -> Bool
canBuyAnObsidianRobot { obsidianCost } { ore, clay } =
    ore >= obsidianCost.ore && clay >= obsidianCost.clay

canBuyAClayRobot : BluePrint -> MineralMap -> Bool
canBuyAClayRobot { clayCost } { ore } =
    ore >= clayCost.ore

canBuyAnOreRobot : BluePrint -> MineralMap -> Bool
canBuyAnOreRobot { oreCost } { ore } =
    ore >= oreCost.ore

useRobots : MineralMap -> MineralMap -> MineralMap
useRobots robots collected =
    { ore = collected.ore + robots.ore
    , clay = collected.clay + robots.clay
    , obsidian = collected.obsidian + robots.obsidian
    , geode = collected.geode + robots.geode
    }

buy : Mineral -> State -> (MineralMap, MineralMap)
buy mineral { blueprint, collected, robots } =
    let
        { oreCost, clayCost, obsidianCost, geodeCost } =
            blueprint
    in
    case mineral of
        Geode ->
            ( { collected | ore = collected.ore - geodeCost.ore, obsidian = collected.obsidian - geodeCost.obsidian }
            , { robots | geode = robots.geode + 1 }
            )
        Obsidian ->
            ( { collected | ore = collected.ore - obsidianCost.ore, clay = collected.clay - obsidianCost.clay }
            , { robots | obsidian = robots.obsidian + 1 }
            )
        Clay ->
            ( { collected | ore = collected.ore - clayCost.ore }
            , { robots | clay = robots.clay + 1 }
            )
        Ore ->
            ( { collected | ore = collected.ore - oreCost.ore }
            , { robots | ore = robots.ore + 1 }
            )

getQualityLevel : State -> Int
getQualityLevel state =
    let
        { minute, collected, blueprint, robots } =
            state
    in
    if minute == 25 then
        collected.geode * blueprint.index
    else
        let
            newMinute =
                minute + 1

            (tempCollected, nextRobots) =
                if canBuyAGeodeRobot blueprint collected then
                    -- always best choice
                    buy Geode state
                else
                    let
                        afterBuyingOsidian =
                            if canBuyAnObsidianRobot blueprint collected then
                                let
                                    (tempCollectedAfterObsidian, robotsAfterObsidian) =
                                        buy Obsidian state
                                    collectedAfterObsidian =
                                        useRobots robots tempCollectedAfterObsidian
                                in
                                getQualityLevel { state
                                    | minute = newMinute
                                    , collected = collectedAfterObsidian
                                    , robots = robotsAfterObsidian
                                    }
                            else
                                collected.geode * blueprint.index
                        
                        afterBuyingClay =
                            if canBuyAClayRobot blueprint collected then
                                let
                                    (tempCollectedAfterClay, robotsAfterClay) =
                                        buy Clay state
                                    collectedAfterClay =
                                        useRobots robots tempCollectedAfterClay
                                in
                                getQualityLevel { state
                                    | minute = newMinute
                                    , collected = collectedAfterClay
                                    , robots = robotsAfterClay
                                    }
                            else
                                collected.geode * blueprint.index

                        afterBuyingOre =
                            if canBuyAnOreRobot blueprint collected then
                                let
                                    (tempCollectedAfterOre, robotsAfterOre) =
                                        buy Ore state
                                    collectedAfterOre =
                                        useRobots robots tempCollectedAfterOre
                                in
                                getQualityLevel { state
                                    | minute = newMinute
                                    , collected = collectedAfterOre
                                    , robots = robotsAfterOre
                                    }
                            else
                                collected.geode * blueprint.index

                    in 
                    
                    if afterBuyingOsidian > afterBuyingClay && afterBuyingOsidian > afterBuyingOre then
                        buy Obsidian state
                    else if afterBuyingClay > afterBuyingOsidian && afterBuyingClay > afterBuyingOre then
                        buy Clay state
                    else if afterBuyingOre > afterBuyingOsidian && afterBuyingOre > afterBuyingClay then
                        buy Ore state
                    else
                        (collected, robots)
            newCollected =
                useRobots robots tempCollected

            
        in
        getQualityLevel { state 
            | minute = newMinute
            , collected = newCollected
            , robots = nextRobots
            }
        

parseInput : String -> List State
parseInput input =
    String.lines input
        |> List.map (Parser.run parseBlueprint)
        |> List.filterMap Result.toMaybe
        |> List.map initialiseState


solveA : String -> String
solveA =
    parseInput >> List.map getQualityLevel >> List.sum >> String.fromInt


solveB : String -> String
solveB input =
    "implement partB here"


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


