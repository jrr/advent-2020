module Tests

open System
open Xunit
open FsUnit.Xunit
open Solve


type Helpers() =

    [<Fact>]
    let parses () =
        Input.exampleInput
        |> parse
        |> should
            equal
               { Fields =
                     [ { Name = "class"
                         Range = ((1, 3), (5, 7)) }
                       { Name = "row"
                         Range = ((6, 11), (33, 44)) }
                       { Name = "seat"
                         Range = ((13, 40), (45, 50)) } ]
                 MyTicket = [ 7; 1; 14 ]
                 NearbyTickets =
                     [ [ 7; 3; 47 ]
                       [ 40; 4; 50 ]
                       [ 55; 2; 20 ]
                       [ 38; 6; 12 ] ] }

type ``Part One``() =
    [<Fact>]
    let ``solves example`` () =
        Input.exampleInput
        |> parse
        |> solveOne
        |> should equal 71



    [<Fact>]
    let ``solves problem`` () =
        Input.problemInput
        |> parse
        |> solveOne
        |> should equal 0

//type ``Part Two``() =
//    [<Fact>]
//    let ``solves example`` () =
//        solveOne Input.exampleInput
//        |> should equal Input.exampleInput
//
//    [<Fact>]
//    let ``solves problem`` () =
//        solveTwo Input.problemInput
//        |> should equal Input.problemInput
