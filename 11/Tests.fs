module Tests

open System
open Xunit
open FsUnit.Xunit
open Solve

[<Fact>]
let ``solves problem`` () = solve "foo" |> should equal "foo"



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
    
type Seat =
    | Floor
    | OpenSeat
    | OccupiedSeat
    
let parseCell = function
    | 'L' -> OpenSeat
    | '.' -> Floor
    | '#' -> OccupiedSeat
    
let unParseCell = function
    | OpenSeat -> 'L'
    | Floor -> '.'
    | OccupiedSeat -> '#'
    
    
let parse (input: string) : Seat array array =
    input |> Common.nonEmptyLines |> Seq.map (fun line ->
        line |> Seq.map (parseCell) |> Array.ofSeq
        ) |> Array.ofSeq
let unparse (input: Seat array array) =
    input |> Seq.map (fun line ->
        line |> Seq.map unParseCell |> String.Concat
        ) |> Seq.reduce (sprintf "%s\n%s") |> sprintf "\n%s\n"
    
    
[<Fact>]
let ``parses board`` () =
    let result = parse Input.exampleInput
    result.[1].[1] |> should equal OpenSeat
    
[<Fact>]
let ``parses small board`` () =
    let result = parse """.L#
#L.
"""
    result |> should equal [|[|Floor;OpenSeat;OccupiedSeat|];[|OccupiedSeat;OpenSeat;Floor|] |]
    
[<Fact>]
let ``unparses small board`` () =
    
    [|[|Floor;OpenSeat;OccupiedSeat|];[|OccupiedSeat;OpenSeat;Floor|] |] |> unparse
        |> should equal """
.L#
#L.
"""
