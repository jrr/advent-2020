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
type AdjacentCell =
    | Neighbor of Seat
    | Wall
    
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

let lookup (input: Seat array array) (y,x) =
    match input |> Array.tryItem y with
    | None -> Wall
    | Some row ->
        match row |> Array.tryItem x with
            | None -> Wall
            | Some seat -> Neighbor seat
            
let directions =  Seq.allPairs [-1;0;1] [-1;0;1]  |> Seq.filter (fun a -> a <> (0,0))

let tupleAdd  a b = (fst a + fst b),(snd a + snd b)

let countNeighbors (input: Seat array array) (coords) =
    directions |> Seq.map (tupleAdd coords) |> Seq.map (lookup input) |> Seq.filter (fun s -> s = Neighbor OccupiedSeat) |> Seq.length
    
let tick (input: Seat array array) =
    input |> Array.mapi (fun y row ->
        row |> Array.mapi (fun x seat ->
                let numNeighbors = countNeighbors input (y,x)
                match (seat,numNeighbors) with
                | OpenSeat,n when n = 0 -> OccupiedSeat
                | OccupiedSeat,n when n >= 4 -> OpenSeat
                | x,_ -> x
            )
        )
[<Fact>]
let ``game of life round 1`` () =
    Input.exampleInput |> parse |> tick |> unparse |> should equal """
#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##
"""

[<Fact>]
let ``game of life round 2`` () =
    """
#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##
"""
    |> parse |> tick |> unparse |> should equal """
#.LL.L#.##
#LLLLLL.L#
L.L.L..L..
#LLL.LL.L#
#.LL.LL.LL
#.LLLL#.##
..L.L.....
#LLLLLLLL#
#.LLLLLL.L
#.#LLLL.##
"""

type Solve11aResult = {NumRounds:int;OccupiedSeats: int}
    
let boardEq (a: Seat array array) (b: Seat array array) =
    Seq.zip a b |> Seq.tryFind (fun (left,right) ->
        let difference = Seq.zip left right |> Seq.tryFind (fun (l,r) -> l <> r)
        match difference with
        | Some x -> true
        | None -> false
        ) |> (function |Some x -> false |None -> true)
    
[<Fact>]
let ``recognizes equivalent boards`` () =
    let b1 = [|[|Floor;OpenSeat;OccupiedSeat|];[|OccupiedSeat;OpenSeat;Floor|] |]
    let b2 = [|[|Floor;OpenSeat;OccupiedSeat|];[|OccupiedSeat;OpenSeat;Floor|] |]
    boardEq b1 b2 |> should equal true
    
[<Fact>]
let ``detects mismatched boards`` () =
    let b1 = [|[|Floor;OpenSeat;OccupiedSeat|];[|OccupiedSeat;Floor;Floor|] |]
    let b2 = [|[|Floor;OpenSeat;OccupiedSeat|];[|OccupiedSeat;OpenSeat;Floor|] |]
    boardEq b1 b2 |> should equal false
    
let  solve (input: Seat array array) =
    {NumRounds=0;OccupiedSeats=0}
    
[<Fact>]
let ``solves 11a example`` () =
    Input.exampleInput |> parse |> solve |> should equal ()
