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

    [<Fact>]
    let factors () =
        6 |> factor |> should equal [ 2; 3 ]
        8 |> factor |> should equal [ 2; 2; 2 ]
        11 |> factor |> should equal [11]
        120 |> factor |> should equal [2;2;2;3;5]
        
    [<Fact>]
    let ``union of two lists`` () =
        mergeFactors [1;2;3] [2;3;4] |> Seq.toList |> should equal [1;2;3;4]
        mergeFactors [1;2;3] [2;2;5] |> Seq.toList |> should equal [1;2;2;3;5]
        
    [<Fact>]
    let ``determines LCM`` () =
        leastCommonMultiple 6 8 |> should equal 24
        ()

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
