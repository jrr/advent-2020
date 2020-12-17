module Tests

open System
open Xunit
open FsUnit.Xunit
open Solve


type ``Helpers`` () =
    
    [<Fact>]
    let ``parses line`` () =
        parseLine "#..#" 2 |> Seq.toList |> should equal [(0,2);(3,2)]
        
    [<Fact>]
    let ``parses input`` () =
        Input.exampleInput
        |> parse
        |> should equal (set [(0, 2); (2, 1); (1, 0); (1, 2); (2, 2)])


type ``Part One`` () =
    [<Fact>]
    let ``solves example`` () =
        solveOne Input.exampleInput |> should equal Input.exampleInput
    [<Fact>]
    let ``solves problem`` () =
        solveOne Input.problemInput |> should equal Input.problemInput
        
type ``Part Two`` () =
    [<Fact>]
    let ``solves example`` () =
        solveTwo Input.exampleInput |> should equal Input.exampleInput
    [<Fact>]
    let ``solves problem`` () =
        solveTwo Input.problemInput |> should equal Input.problemInput
