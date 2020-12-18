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
        |> (neighborsOf neighborVectors3 addPoint3)
        |> Set.count
        |> should equal 26

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
        Input.exampleInput
        |> solvePartOne
        |> should equal 112

    [<Fact>]
    let ``solves problem`` () =
        Input.problemInput
        |> solvePartOne
        |> should equal 202

type ``Part Two``() =
    [<Fact>]
    let ``solves example 2D`` () =
        Input.exampleInput |> solve2d |> should equal 5

    [<Fact>]
    let ``solves example`` () =
        Input.exampleInput
        |> solvePartTwo
        |> should equal 848

    [<Fact>]
    let ``solves problem`` () =
        Input.problemInput
        |> solvePartTwo
        |> should equal 2028
