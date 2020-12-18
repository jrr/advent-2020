module Tests

open System
open Xunit
open FsUnit.Xunit
open Solve


type Helpers() =

    [<Fact>]
    let ``parses line`` () =
        parseLine "#..#" 2
        |> Seq.toList
        |> should equal [ (0, 2, 0); (3, 2, 0) ]

    [<Fact>]
    let ``parses input`` () =
        Input.exampleInput
        |> parse
        |> should
            equal
               (set [ (0, 2, 0)
                      (1, 0, 0)
                      (1, 2, 0)
                      (2, 1, 0)
                      (2, 2, 0) ])


    [<Fact>]
    let prints () =
        Input.exampleInput
        |> parse
        |> print
        |> should equal """z=0
.#.
..#
###
"""

    [<Fact>]
    let neighbors () =
        (3, 3, 3)
        |> (neighbors point3ops)
        |> Set.count
        |> should equal 26

    [<Fact>]
    let times () =
        times 2 (fun x -> x + 1) 5 |> should equal 7

type ``Part One``() =
    [<Fact>]
    let ``one round of example`` () =
        Input.exampleInput
        |> parse
        |> tick3
        |> print
        |> should equal """z=-1
#..
..#
.#.

z=0
#.#
.##
.#.

z=1
#..
..#
.#.
"""

        ()

    [<Fact>]
    let ``solves example`` () =
        Input.exampleInput |> solveOne |> should equal 112

    [<Fact>]
    let ``solves problem`` () =
        Input.problemInput |> solveOne |> should equal 202

type ``Part Two``() =
    [<Fact>]
    let ``solves example 2D`` () =
        Input.exampleInput |> solve2d |> should equal 5
        
    [<Fact>]
    let ``solves example`` () =
        solveTwo Input.exampleInput
        |> should equal Input.exampleInput

    [<Fact>]
    let ``solves problem`` () =
        solveTwo Input.problemInput
        |> should equal Input.problemInput
