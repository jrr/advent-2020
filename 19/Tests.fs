module Tests

open System
open System.Text.RegularExpressions
open Xunit
open FsUnit.Xunit
open Solve

type Helpers() =


    [<Fact>]
    let ``parses rules`` () =
        "60: 117 7 | 89 13" |>parseRule|> should equal ("60", Or ([117; 7], [89; 13]))
        "64: 99 7"|>parseRule|>should equal ("64", Digits [99; 7])
        "13: \"b\"" |>parseRule |> should equal ("13", StringRule "b")
        
    [<Fact>]
    let ``replaces single-digit numbers with letters`` () =
        
//        7: "a"
        let input = """5: 108 13 | 27 7
30: 14 7 | 50 13
7: but not this one
50: 13 23 | 7 111
32: 13 7 | 13 13
92: 114 13 | 125 7"""
//        input.repl
        let result = replaceNumWithString input 7 "a"
        result |> should equal """5: 108 13 | 27 a
30: 14 a | 50 13
7: but not this one
50: 13 23 | a 111
32: 13 a | 13 13
92: 114 13 | 125 7"""

    [<Fact>]
    let ``replaces two-digit numbers with letters`` () =
        
//        7: "a"
        let input = """5: 108 13 | 27 7
30: 14 7 | 50 13
50: 13 23 | 7 111
32: 13 7 | 13 13
92: 114 13 | 125 7"""
//        input.repl
        let result = replaceNumWithString input 13 "b"
        result |> should equal "5: 108 b | 27 7
30: 14 7 | 50 b
50: b 23 | 7 111
32: b 7 | b 13
92: 114 b | 125 7"
        
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
