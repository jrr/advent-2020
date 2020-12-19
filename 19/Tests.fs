module Tests

open System
open System.Text.RegularExpressions
open Xunit
open FsUnit.Xunit
open Solve

type TreeStuff() =

    [<Fact>]
    let ``builds tree`` () =
        let ruleMap =
            buildRuleMap (Input.exampleInput |> parse).rules

        let expected =
            SequenceNode [ LeafNode "a"
                           OrNode [ SequenceNode [ OrNode [ SequenceNode [ LeafNode "a"
                                                                           LeafNode "a" ]
                                                            SequenceNode [ LeafNode "b"
                                                                           LeafNode "b" ] ]
                                                   OrNode [ SequenceNode [ LeafNode "a"
                                                                           LeafNode "b" ]
                                                            SequenceNode [ LeafNode "b"
                                                                           LeafNode "a" ] ] ]
                                    SequenceNode [ OrNode [ SequenceNode [ LeafNode "a"
                                                                           LeafNode "b" ]
                                                            SequenceNode [ LeafNode "b"
                                                                           LeafNode "a" ] ]
                                                   OrNode [ SequenceNode [ LeafNode "a"
                                                                           LeafNode "a" ]
                                                            SequenceNode [ LeafNode "b"
                                                                           LeafNode "b" ] ] ] ]
                           LeafNode "b" ]

        buildTree ruleMap 0 |> should equal expected
        ()

type Helpers() =


    [<Fact>]
    let ``parses rules`` () =
        "60: 117 7 | 89 13"
        |> parseRule
        |> should equal (60, Or([ Num 117; Num 7 ], [ Num 89; Num 13 ]))
        "64: 99 7"
        |> parseRule
        |> should equal (64, Digits [ Num 99; Num 7 ])
        "13: \"b\""
        |> parseRule
        |> should equal (13, StringRule "b")

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
        result
        |> should equal """5: 108 13 | 27 a
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
        result
        |> should equal "5: 108 b | 27 7
30: 14 7 | 50 b
50: b 23 | 7 111
32: b 7 | b 13
92: 114 b | 125 7"

    [<Fact>]
    let ``parses example input`` () =
        Input.exampleInput
        |> parse
        |> should
            equal
               { rules =
                     [ (0, Digits [ Num 4; Num 1; Num 5 ])
                       (1, Or([ Num 2; Num 3 ], [ Num 3; Num 2 ]))
                       (2, Or([ Num 4; Num 4 ], [ Num 5; Num 5 ]))
                       (3, Or([ Num 4; Num 5 ], [ Num 5; Num 4 ]))
                       (4, StringRule "a")
                       (5, StringRule "b") ]
                 messages =
                     [ "ababbb"
                       "bababa"
                       "abbbab"
                       "aaabbb"
                       "aaaabbb" ] }

    [<Fact>]
    let ``applies string`` () =
        let input = [ (3, Digits [ Num 4; Num 5; Num 6 ]) ]

        let expected =
            [ (3, Digits [ Num 4; String "X"; Num 6 ]) ]

        applyStringRule (5, "X") input
        |> List.ofSeq
        |> should equal expected

    [<Fact>]
    let applyStringToDigitList () =
        applyStringToDigitList [ Num 4; Num 4 ] (4, "X")
        |> should equal [ String "X"; String "X" ]

    [<Fact>]
    let ``reduces via string replacement`` () =
        let input =
            [ (0, Digits [ Num 4; Num 1; Num 5 ])
              (1, Or([ Num 2; Num 4 ], [ Num 4; Num 2 ]))
              (4, StringRule "a") ]

        let expected =
            [ (0, Digits [ String "a"; Num 1; Num 5 ])
              (1, Or([ Num 2; String "a" ], [ String "a"; Num 2 ])) ]

        reduceStringRules input |> should equal expected

    [<Fact>]
    let ``reduces digits`` () =
        [ Num 1
          Num 2
          String "a"
          String "b"
          Num 3 ]
        |> reduceDigits
        |> should equal [ Num 1; Num 2; String "ab"; Num 3 ]

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
