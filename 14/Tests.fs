module Tests

open System
open Xunit
open FsUnit.Xunit
open Solve


type Helpers() =

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

    [<Fact>]
    let ``parses input`` () =
        Input.exampleInput
        |> parse
        |> List.ofSeq
        |> should
            equal
               [ Mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
                 Write(8u, 11UL)
                 Write(7u, 101UL)
                 Write(8u, 0UL) ]
               
    [<Fact>]
    let ``parses binary`` () =
        "101" |> parseBinary |> should equal 5UL
        
    [<Fact>]
    let ``applies mask`` () =
        applyMask 11UL "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" |> should equal 73UL
        applyMask 00UL "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" |> should equal 64UL
        

type ``Part One``() =
    [<Fact>]
    let ``solves example`` () =
        solveOne Input.exampleInput
        |> should equal Input.exampleInput

    [<Fact>]
    let ``solves problem`` () =
        solveTwo Input.problemInput
        |> should equal Input.problemInput

type ``Part Two``() =
    [<Fact>]
    let ``solves example`` () =
        solveOne Input.exampleInput
        |> should equal Input.exampleInput

    [<Fact>]
    let ``solves problem`` () =
        solveTwo Input.problemInput
        |> should equal Input.problemInput
