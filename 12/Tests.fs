module Tests

open System
open Xunit
open FsUnit.Xunit
open Solve

[<Fact>]
let ``solves problem`` () = solve "foo" |> should equal "foo"



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

type ``Part One``() =
    [<Fact>]
    let ``parses line`` () =
        Input.exampleInput
        |> Common.nonEmptyLines
        |> Seq.head
        |> parseLine
        |> should equal ('F', 10)

    [<Fact>]
    let ``solves example`` () =
        Input.exampleInput
        |> Common.nonEmptyLines
        |> Seq.map parseLine
        |> solveOne
        |> should equal 25

    [<Fact>]
    let ``solves problem`` () =
        Input.problemInput
        |> Common.nonEmptyLines
        |> Seq.map parseLine
        |> solveOne
        |> should equal 1687

type ``Part Two``() =
    [<Fact>]
    let ``reduce 1`` () =
        reduce initialStateTwo ('F', 10)
        |> should
            equal
               { initialStateTwo with
                     ShipPos = { X = 100; Y = 10 } }

    [<Fact>]
    let ``reduce 2`` () =
        reduce
            { ShipPos = { X = 100; Y = 10 }
              Waypoint = { X = 10; Y = 1 } }
            ('N', 3)
        |> should
            equal
               { ShipPos = { X = 100; Y = 10 }
                 Waypoint = { X = 10; Y = 4 } }

    [<Fact>]
    let rot90 () =
        rot90 { X = 10; Y = 4 }
        |> should equal { X = 4; Y = 0 - 10 }
        rot90 { X = 1; Y = 0 - 2 }
        |> should equal { X = 0 - 2; Y = 0 - 1 }

    [<Fact>]
    let ``reduce 3`` () =
        reduce
            { ShipPos = { X = 100; Y = 10 }
              Waypoint = { X = 10; Y = 4 } }
            ('R', 90)
        |> should
            equal
               { ShipPos = { X = 100; Y = 10 }
                 Waypoint = { X = 4; Y = 0 - 10 } }

    [<Fact>]
    let ``solves example`` () =
        Input.exampleInput
        |> Common.nonEmptyLines
        |> Seq.map parseLine
        |> solveTwo
        |> should equal 286

    [<Fact>]
    let ``solves problem`` () =
        Input.problemInput
        |> Common.nonEmptyLines
        |> Seq.map parseLine
        |> solveTwo
        |> should equal 20873
