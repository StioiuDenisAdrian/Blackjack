module Test.Generated.Main1912419751 exposing (main)

import Tests
import Example

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    [     Test.describe "Tests" [Tests.suite],     Test.describe "Example" [Example.suite] ]
        |> Test.concat
        |> Test.Runner.Node.run { runs = Nothing, report = (ConsoleReport UseColor), seed = 337700748094511, processes = 8, globs = [], paths = ["C:\\Users\\Denis\\Desktop\\Functional Programming\\Project (1)\\tests\\Example.elm","C:\\Users\\Denis\\Desktop\\Functional Programming\\Project (1)\\tests\\Tests.elm"]}