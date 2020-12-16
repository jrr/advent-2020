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

    [<Fact>]
    let ``transforms rows into columns`` () =
        [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ]
        |> colsFromRows
        |> should equal [ [ 1; 4 ]; [ 2; 5 ]; [ 3; 6 ] ]

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
        |> should equal 22073

type ``Part Two``() =
    [<Fact>]
    let ``validates tickets`` () =
        Input.exampleInput
        |> parse
        |> filterOutInvalidTickets
        |> (fun i -> i.NearbyTickets.Length)
        |> should equal 1

    [<Fact>]
    let ``solves example`` () =
        Input.examplePart2
        |> parse
        |> solveTwo
        |> set
        |> should
            equal
               (set [ "class", 12
                      "row", 11
                      "seat", 13 ])

    [<Fact>]
    let ``solves problem`` () =
        Input.problemInput
        |> parse
        |> solveTwo
        |> myTicketProduct
        |> should equal 1346570764607L
