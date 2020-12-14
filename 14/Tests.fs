module Tests

open System
open Xunit
open FsUnit.Xunit
open Solve


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
    let ``solves example`` () =
        solveOne Input.exampleInput |> should equal Input.exampleInput
    [<Fact>]
    let ``solves problem`` () =
        solveTwo Input.problemInput |> should equal Input.problemInput
        
type ``Part Two`` () =
    [<Fact>]
    let ``solves example`` () =
        solveOne Input.exampleInput |> should equal Input.exampleInput
    [<Fact>]
    let ``solves problem`` () =
        solveTwo Input.problemInput |> should equal Input.problemInput
