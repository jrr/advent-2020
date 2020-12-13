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
    let parses () =
        Input.exampleInput
        |> parse
        |> should
            equal
               { earliestDeparture = 939
                 buses =
                     [ BusId 7
                       BusId 13
                       OutOfService
                       OutOfService
                       BusId 59
                       OutOfService
                       BusId 31
                       BusId 19 ] }

type ``Part One``() =
    [<Fact>]
    let ``solves example`` () =
        (* The earliest bus you could take is bus ID 59. It doesn't depart until timestamp 944, so you would need to wait 944 - 939 = 5 minutes before it departs. Multiplying the bus ID by the number of minutes you'd need to wait gives 295. *)
        Input.exampleInput
        |> parse
        |> solveOne
        |> should equal 295

    [<Fact>]
    let ``solves problem`` () =
        Input.problemInput
        |> parse
        |> solveOne
        |> should equal 104

type ``Part Two``() =
    [<Fact>]
    let ``bus positions`` () =
        Input.exampleInput
        |> parse
        |> busPositions
        |> should
            equal
               [ { Position = 0; BusId = 7 }
                 { Position = 1; BusId = 13 }
                 { Position = 4; BusId = 59 }
                 { Position = 6; BusId = 31 }
                 { Position = 7; BusId = 19 } ]

    [<Fact>]
    let ``solves example`` () =
        Input.exampleInput
        |> parse
        |> solveTwo
        |> should equal 1068781L

    [<Fact(Skip = "too slow")>]
    let ``solves problem`` () =
        // definitely larger than 1516900000
        Input.problemInput
        |> parse
        |> solveTwo
        |> should equal 0
