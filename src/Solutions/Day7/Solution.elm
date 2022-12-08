module Solutions.Day7.Solution exposing (..)

{-| -}

import Solutions.Day7.Input as Input



type Children
    = Children (List Node)


type alias Node =
    { label : String
    , size : Int
    , children : Children
    , parents : List String
    }


type alias ParseState =
    { fileSystem : Node
    , currentDir : List String
    }


type Command
    = Cd String
    | UpOneLevel
    | Home


runCommand : Command -> ParseState -> ParseState
runCommand command state =
    case command of
        Cd dir ->
            { state | currentDir = dir :: state.currentDir }

        UpOneLevel ->
            case state.currentDir of
                _ :: rest ->
                    { state | currentDir = rest }

                [] ->
                    state

        Home ->
            { state | currentDir = [ "/" ] }


findCurrent : ParseState -> Maybe Node
findCurrent state =
    findDirectoryInFilesystem (List.reverse state.currentDir) (Children [ state.fileSystem ])


replaceChild : Children -> Node -> Children
replaceChild (Children children) node =
    let
        otherChildren =
            List.filter (\{ label } -> label /= node.label) children
    in
    Children (node :: otherChildren)


addChild : Node -> ParseState -> ParseState
addChild child state =
    case findCurrent state of
        Just current ->
            let
                currentWithChild =
                    { current | children = replaceChild current.children child }
            in
            addNodeToFileSystem currentWithChild state

        Nothing ->
            state



{--find in the entire filesystem - assumes the parent list starts from the top level --}


findDirectoryInFilesystem : List String -> Children -> Maybe Node
findDirectoryInFilesystem parents nodes =
    case parents of
        [] ->
            Nothing

        parent :: [] ->
            findDirectory parent nodes

        parent :: rest ->
            findDirectory parent nodes
                |> Maybe.andThen (\found -> findDirectoryInFilesystem rest found.children)



{--find the given child of the given node only (one level) --}


findDirectory : String -> Children -> Maybe Node
findDirectory label (Children children) =
    case children of
        child :: rest ->
            if child.label == label then
                Just child

            else
                findDirectory label (Children rest)

        [] ->
            Nothing


addNodeToFileSystem : Node -> ParseState -> ParseState
addNodeToFileSystem node state =
    if node.parents == [] then
        
        { state | fileSystem = node }

    else
        case findDirectoryInFilesystem (List.reverse node.parents) (Children [ state.fileSystem ]) of
            Just parentDir ->
                addNodeToFileSystem { parentDir | children = replaceChild parentDir.children node } state

            Nothing ->
                state


getAllNodes : Node -> List Node
getAllNodes node =
    node
        :: List.concatMap
            getAllNodes
            (getNodeChildren node)


filterDirectories : List Node -> List Node
filterDirectories =
    List.filter (\node -> not (List.isEmpty (getNodeChildren node)))


parseLine : String -> ParseState -> ParseState
parseLine line state =
    case String.split " " line of
        [ "$", "ls" ] ->
            state

        [ "$", "cd", "/" ] ->
            runCommand Home state

        [ "$", "cd", ".." ] ->
            runCommand UpOneLevel state

        [ "$", "cd", label ] ->
            { state | currentDir = label :: state.currentDir }

        [ "dir", label ] ->
            addChild { size = 0, children = Children [], label = label, parents = state.currentDir } state

        [ size, label ] ->
            case String.toInt size of
                Just sizeInt ->
                    addChild { size = sizeInt, children = Children [], label = label, parents = state.currentDir } state

                Nothing ->
                    state

        _ ->
            state


getNodeChildren : Node -> List Node
getNodeChildren { children } =
    case children of
        Children nodes ->
            nodes


sumAllSizesBelowMaximum : Int -> List Int -> Int
sumAllSizesBelowMaximum max remainders =
    List.map (\n -> max - n) remainders
        |> List.sum


parseInput : String -> ParseState
parseInput input =
    let
        homeDir =
            { label = "/"
            , children = Children []
            , parents = []
            , size = 0
            }
    in
    String.lines input
        |> List.foldl
            parseLine
            { fileSystem = homeDir
            , currentDir = [ "/" ]
            }


getSize : Node -> Int
getSize node =
    let
        sizeOfChildren =
            List.map getSize (getNodeChildren node)
                |> List.sum
    in
    node.size + sizeOfChildren


solveA : String -> String
solveA input =
    let
        parsedInput =
            parseInput input

        allDirectories =
            getAllNodes parsedInput.fileSystem
                |> filterDirectories
    in
    List.map getSize allDirectories
        |> List.filter (\size -> size <= 100000)
        |> List.sum
        |> String.fromInt


solveB : String -> String
solveB input =
    let
        parsedInput =
            parseInput input

        allDirectories =
            getAllNodes parsedInput.fileSystem
                |> filterDirectories

        totalSize =
            getSize parsedInput.fileSystem

        currentFreeSpace =
            70000000 - totalSize

        requiredSpace =
            30000000 - currentFreeSpace
    in
    List.map getSize allDirectories
        |> List.filter (\size -> size >= requiredSpace)
        |> List.minimum
        |> Maybe.withDefault 0 
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
