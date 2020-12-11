module Solve
open System

type Seat =
    | Floor
    | OpenSeat
    | OccupiedSeat
type SeatLayout = Seat array array
type AdjacentCell =
    | Neighbor of Seat
    | Wall
    
        
type Solve11aResult = {NumRounds:int;OccupiedSeats: int}
    
let boardEq (a: SeatLayout) (b: SeatLayout) =
    Seq.zip a b |> Seq.tryFind (fun (left,right) ->
        let difference = Seq.zip left right |> Seq.tryFind (fun (l,r) -> l <> r)
        match difference with
        | Some x -> true
        | None -> false
        ) |> (function |Some x -> false |None -> true)
    
let countOccupied (input: SeatLayout) =
    input |> Seq.concat |> Seq.filter (fun s -> s = OccupiedSeat) |> Seq.length
                                       
let lookup (input: SeatLayout) (y,x) =
    match input |> Array.tryItem y with
    | None -> Wall
    | Some row ->
        match row |> Array.tryItem x with
            | None -> Wall
            | Some seat -> Neighbor seat
            
let directions =  Seq.allPairs [-1;0;1] [-1;0;1]  |> Seq.filter (fun a -> a <> (0,0))

let tupleAdd  a b = (fst a + fst b),(snd a + snd b)

let countNeighbors (input: SeatLayout) (coords) =
    directions |> Seq.map (tupleAdd coords) |> Seq.map (lookup input) |> Seq.filter (fun s -> s = Neighbor OccupiedSeat) |> Seq.length
    
let tick (input: SeatLayout) =
    input |> Array.mapi (fun y row ->
        row |> Array.mapi (fun x seat ->
                let numNeighbors = countNeighbors input (y,x)
                match (seat,numNeighbors) with
                | OpenSeat,n when n = 0 -> OccupiedSeat
                | OccupiedSeat,n when n >= 4 -> OpenSeat
                | x,_ -> x
            )
        )
let rec solveRec (input: SeatLayout) (numRounds:int)=
    let after = tick input
    let count = countOccupied input
    if boardEq input after then
        {NumRounds=numRounds;OccupiedSeats=count}
    else
        solveRec after (numRounds+1)
    
let solve (input: SeatLayout) =
    solveRec input 0
    
let parseCell = function
    | 'L' -> OpenSeat
    | '.' -> Floor
    | '#' -> OccupiedSeat
    
let unParseCell = function
    | OpenSeat -> 'L'
    | Floor -> '.'
    | OccupiedSeat -> '#'
    
    
let parse (input: string) : SeatLayout =
    input |> Common.nonEmptyLines |> Seq.map (fun line ->
        line |> Seq.map (parseCell) |> Array.ofSeq
        ) |> Array.ofSeq
let unparse (input: SeatLayout) =
    input |> Seq.map (fun line ->
        line |> Seq.map unParseCell |> String.Concat
        ) |> Seq.reduce (sprintf "%s\n%s") |> sprintf "\n%s\n"
    
    
