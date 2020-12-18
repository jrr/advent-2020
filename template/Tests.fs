module Tests

open System
open Xunit
open FsUnit.Xunit
open Solve

type Helpers() =

    [<Fact>]
    let ``parses example input`` () =
        Input.exampleInput
        |> parse
        |> should not' (equal true)
        ()

type ``Part One``() =
    [<Fact>]
    let ``solves example`` () =
        Input.exampleInput
        |> solveOne
        |> should not' (equal true)

    [<Fact>]
    let ``solves problem`` () =
        Input.problemInput
        |> solveOne
        |> should not' (equal true)

type ``Part Two``() =
    [<Fact>]
    let ``solves example`` () =
        Input.exampleInput
        |> solveTwo
        |> should not' (equal true)

    [<Fact>]
    let ``solves problem`` () =
        Input.problemInput
        |> solveTwo
        |> should not' (equal true)
