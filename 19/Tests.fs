module Tests

open System
open System.Text.RegularExpressions
open Xunit
open FsUnit.Xunit
open Solve

type Rule =
    | StringRule of string
    | Digits of int list
    | Or of (int list) * (int list)
    
type ProblemInput = {
    rules: (int * Rule) list
    messages: string list
}
type Helpers() =


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
        result |> should equal ""
        
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
