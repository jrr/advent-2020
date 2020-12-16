module Tests

open Xunit
open FsUnit.Xunit
open Solve

type ``Part One``() =
    [<Fact>]
    let ``example sequence`` () =
        Input.exampleInput
        |> vanEckSeq
        |> Seq.take 10
        |> List.ofSeq
        |> should equal [ 0; 3; 6; 0; 3; 3; 1; 0; 4; 0 ]

    [<Fact>]
    let ``solves small example`` () =
        Input.exampleInput |> solve15a |> should equal 436

    [<Fact>]
    let ``more examples`` () =
        [ 1; 3; 2 ] |> solve15a |> should equal 1
        [ 2; 1; 3 ] |> solve15a |> should equal 10
        [ 1; 2; 3 ] |> solve15a |> should equal 27
        [ 2; 3; 1 ] |> solve15a |> should equal 78
        [ 3; 2; 1 ] |> solve15a |> should equal 438
        [ 3; 1; 2 ] |> solve15a |> should equal 1836

    [<Fact>]
    let ``solve 15a`` () =
        Input.problemInput |> solve15a |> should equal 240

type ``Part Two``() =
    [<Fact>]
    let ``solves example`` () =
        Input.exampleInput
        |> solve15b
        |> should equal 175594

    [<Fact>]
    let ``solves problem`` () =
        Input.problemInput |> solve15b |> should equal 505
