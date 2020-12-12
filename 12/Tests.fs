module Tests

open System
open Xunit
open FsUnit.Xunit
open Solve

[<Fact>]
let ``solves problem`` () = solve "foo" |> should equal "foo"



type ``Helpers`` () =
    
    [<Fact>]
    let ``reads lines of text`` () =
        Input.inputLines
        |> Common.nonEmptyLines
        |> Seq.length
        |> should greaterThan 3

    [<Fact>]
    let ``reads groups of lines of text`` () =
        Input.inputGroups
        |> Common.lineGroups
        |> Seq.length
        |> should equal 2

type ``Part One`` () =
    [<Fact>]
    let ``parses line`` () =
        Input.exampleInput |> Common.nonEmptyLines |> Seq.head |> parseLine |> should equal ("F",10)
        
    [<Fact>]
    let ``solves example`` () =
        let instructions = Input.exampleInput |> Common.nonEmptyLines |> Seq.map parseLine
        solveOne instructions |> should equal 25
    [<Fact>]
    let ``solves problem`` () =
        let instructions = Input.problemInput |> Common.nonEmptyLines |> Seq.map parseLine
        solveOne instructions |> should equal 1687
        
type ``Part Two`` () =
    [<Fact>]
    let ``solves example`` () =
        solveTwo Input.exampleInput |> should equal Input.exampleInput
    [<Fact>]
    let ``solves problem`` () =
        solveTwo Input.problemInput |> should equal Input.problemInput
