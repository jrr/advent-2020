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
    let ``evaluates single addition`` () =
        "5+2" |> evalString |> should equal "7"
    [<Fact>]
    let ``evaluates multiple operations`` () =
        "1+2*3" |> evalString |> should equal "9"
        
    [<Fact>]
    let ``solves with parens`` () =
        "1+(2*3)" |> recSolve |> should equal "7"
        
    [<Fact>]
    let ``tokenizes simple case`` () =
        "1+2" |>tokenize |> should equal [IntLiteral 1L; Plus; IntLiteral 2L]
    [<Fact>]
    let ``tokenizes larger number`` () =
        "123" |>tokenize |> should equal [IntLiteral 123L]
        
    

type ``Part One``() =
//    [<Fact>]
//    let ``tokenizes string`` () =
//        "2 * 3 + (4 * 5)"
//        |> parse
//        |> should
//            equal
//               [ IntLiteral 2
//                 MultipliedBy
//                 IntLiteral 3
//                 Plus
//                 LeftParen
//                 IntLiteral 4
//                 MultipliedBy
//                 IntLiteral 5
//                 RightParen ]

    [<Fact>]
    let ``counts parens`` () =
        "123" |> countParens |> should equal NoParens
        "1(2)3" |> countParens |> should equal (HasParens 1)
        "(1(2)3)" |> countParens |> should equal (HasParens 2)
        
    [<Fact>]
    let ``extracts single parenthesized sequence`` () =
        "1+2+(3*4)+5" |> extractParenthesized |> should equal {left="1+2+";parenthesized="3*4";right="+5" }
        
    [<Fact>]
    let ``extracts two parenthesized sequences`` () =
        "1+2+(3*(4+5))" |> extractParenthesized |> should equal { left = "1+2+(3*"; parenthesized = "4+5"; right = ")" }
        
    [<Fact>]
    let ``solves example lines`` () =
        "2 * 3 + (4 * 5)" |> solveLine |> should equal 26L
        "5 + (8 * 3 + 9 + 3 * 4 * 3)" |> solveLine |> should equal 437L
        "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" |>solveLine |> should equal 12240L
        "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" |> solveLine |> should equal 13632L
        
    [<Fact>]
    let ``solves problem`` () =
        Input.problemInput |> solveOne |> should equal 5019432542701L

type ``Part Two``() =
    [<Fact>]
    let ``solves example`` () =
        solveTwo Input.exampleInput
        |> should equal Input.exampleInput

    [<Fact>]
    let ``solves problem`` () =
        solveTwo Input.problemInput
        |> should equal Input.problemInput
