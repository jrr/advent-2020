module Tests

open System
open Xunit
open FsUnit.Xunit
open Solve

[<Fact>]
let ``solves problem`` () = solve "foo" |> should equal "foo"



let input = """FBBBBBBLRR
FFBBBBFRRL
FFBBBFBRLR
BFFBFBFRLL
BFFFBBFRRR
FFBFBFFRRR
BFFBBBBRLL"""

[<Fact>]
let stuff () =
    input.Split('\n')
    |> Seq.length
    |> should greaterThan 3
