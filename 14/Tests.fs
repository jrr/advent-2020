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
    let ``parses input`` () =
        Input.exampleInput
        |> parse
        |> List.ofSeq
        |> should
            equal
               [ MaskLine "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
                 WriteLine { Address = 8UL; Value = 11UL }
                 WriteLine { Address = 7UL; Value = 101UL }
                 WriteLine { Address = 8UL; Value = 0UL } ]

    [<Fact>]
    let ``parses binary`` () = "101" |> parseBinary |> should equal 5UL

    [<Fact>]
    let ``prints binary`` () =
        5UL |> printBinary |> should endWith "0101"

    [<Fact>]
    let ``applies mask`` () =
        computeValueMask 11UL "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
        |> should equal 73UL
        computeValueMask 00UL "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
        |> should equal 64UL

    [<Fact>]
    let ``applies value masks`` () =

        [ { Mask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
            Write = { Address = 8UL; Value = 11UL } }
          { Mask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
            Write = { Address = 7UL; Value = 101UL } }
          { Mask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
            Write = { Address = 8UL; Value = 0UL } } ]

        |> List.map applyValueMask
        |> List.concat
        |> should
            equal
               [ { Address = 8UL; Value = 73UL }
                 { Address = 7UL; Value = 101UL }
                 { Address = 8UL; Value = 64UL } ]

    [<Fact>]
    let ``reduces redundant writes`` () =
        [ { Address = 8UL; Value = 73UL }
          { Address = 7UL; Value = 101UL }
          { Address = 8UL; Value = 64UL } ]
        |> reduceWrites
        |> Seq.toList
        |> should
            equal
               [ { Address = 7UL; Value = 101UL }
                 { Address = 8UL; Value = 64UL } ]

    [<Fact>]
    let ``applies address mask`` () =
        { Mask = "000000000000000000000000000000X1001X"
          Write = { Address = 42UL; Value = 100UL } }
        |> applyAddressMask
        |> Seq.map (fun x -> x.Address)
        |> List.ofSeq
        |> should equal [ 26UL; 27UL; 58UL; 59UL ]

type ``Part One``() =
    [<Fact>]
    let ``solves example`` () =
        solveOne Input.exampleInput |> should equal 165UL

    [<Fact>]
    let ``solves problem`` () =
        solveOne Input.problemInput
        |> should equal 7817357407588UL

type ``Part Two``() =
    [<Fact>]
    let ``solves example`` () =
        solveTwo Input.exampleInput14b
        |> should equal 208UL

    [<Fact>]
    let ``solves problem`` () =
        solveTwo Input.problemInput
        |> should equal 4335927555692UL
