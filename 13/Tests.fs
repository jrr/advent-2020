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
    [<Fact>]
    let ``parses`` () =
        Input.exampleInput |> parse |> should equal { earliestDeparture = 939; buses = [BusId 7; BusId 13; OutOfService; OutOfService; BusId 59; OutOfService; BusId 31; BusId 19] }

type ``Part One`` () =
    [<Fact>]
    let ``solves example`` () =
        (* The earliest bus you could take is bus ID 59. It doesn't depart until timestamp 944, so you would need to wait 944 - 939 = 5 minutes before it departs. Multiplying the bus ID by the number of minutes you'd need to wait gives 295. *)
        Input.exampleInput |> parse |> solveOne |> should equal 295
    [<Fact>]
    let ``solves problem`` () =
        Input.problemInput |> parse |> solveOne |> should equal 0
        
type ``Part Two`` () =
    [<Fact>]
    let ``solves example`` () =
        Input.exampleInput |> parse |> solveTwo |> should equal Input.exampleInput
    [<Fact>]
    let ``solves problem`` () =
        Input.problemInput |> parse |> solveTwo |> should equal Input.problemInput
