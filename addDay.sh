IFS=
export $(grep -v '#.*' .env | xargs)

curl -b "session=$GITHUB_COOKIE" https://adventofcode.com/2022/day/$1/input > input.txt

mkdir src/Solutions/Day$1
npx elm-codegen run codegen/GenerateInputFile.elm --flags-from=input.txt --output=helpers
npx elm-codegen install
npx elm-codegen run codegen/GenerateSolutionFile.elm --output=helpers
npx elm-codegen install
npx elm-codegen run codegen/GenerateTestFile.elm --output=src/Solutions/Day$1

# fix up module names and imports
sed -i "" "s,module Input,module Solutions.Day$1.Input,g" "src/Solutions/Day$1/Input.elm"
sed -i "" "s,module Solution,module Solutions.Day$1.Solution,g" "src/Solutions/Day$1/Solution.elm"
sed -i "" "s,import Input,import Solutions.Day$1.Input as Input,g" "src/Solutions/Day$1/Solution.elm"
sed -i "" "s,module Test,module Solutions.Day$1.Test,g" "src/Solutions/Day$1/Test.elm"
sed -i "" "s,import Solution,import Solutions.Day$1.Solution as Solution,g" "src/Solutions/Day$1/Test.elm"
sed -i "" "s,import Input,import Solutions.Day$1.Input as Input,g" "src/Solutions/Day$1/Test.elm"

rm input.txt

echo "Success!!\n"

echo "Next steps:\n"
echo "  1) add your test input and expected test answer for part A to src/Solutions/Day$1/Input.elm"
echo "  2) run \"npx elm-test --watch src/Solutions/Day$1/Test.elm\""
echo "  3) implement your solution until the test passes"
echo "    a) to test your solution while you work, run \"elm repl\""
echo "    b) \"import Solutions.Day$1.Solution exposing (..)\""
echo "    c) \"partA \"test\"\" or \"partA \"input\"\""
echo "  4) repeat steps 1-3 for part B 🥲"