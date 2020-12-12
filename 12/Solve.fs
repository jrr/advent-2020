module Solve

open System

let solve (input: string) = input


type Coords = {X:int;Y:int}

open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let parseLine (s: string) =
    match s with
    | Regex @"(\D)(\d+)" [ letter; num ] -> (letter.[0], int num)
    | _ -> failwith "parse error"

type ShipState = { (* 0 is north, 90 is east *)Facing: int; Coords:Coords}

let initialShipState = {Facing=90;Coords={X=0;Y=0}}

let add (a:Coords) (b:Coords) = {X=a.X+b.X;Y=a.Y+b.Y}
let goForward state i =
    match state.Facing with
    | 0 -> {state with Coords = add state.Coords {X=0;Y=i}}
    | 90 -> {state with Coords = add state.Coords {X=i;Y=0}}
    | 180 -> {state with Coords = add state.Coords {X=0;Y=0-i}}
    | 270 -> {state with Coords = add state.Coords {X=0-i;Y=0}}
    
    
let rec go (state:ShipState) (instructions: (char*int) seq) =
    let tail = instructions |> Seq.tail
    
    match instructions |> Seq.tryHead with
    | None -> state
    | Some ('N',i) -> go {state with Coords = add state.Coords {X=0;Y=i}} tail
    | Some ('E',i) -> go {state with Coords = add state.Coords {X=i;Y=0}}tail
    | Some ('S',i) -> go {state with Coords = add state.Coords {X=0;Y=0-i}} tail
    | Some ('W',i) -> go {state with Coords = add state.Coords {X=0-i;Y=0}} tail
    | Some ('L',i) -> go {state with Facing = ((state.Facing + 360 - i) % 360) } tail
    | Some ('R',i) -> go {state with Facing = ((state.Facing + 360 + i) % 360) } tail
    | Some ('F',i) -> go (goForward state i) tail
    | instr -> failwith (sprintf "unhandled instruction %O" instr)
    
    
let abs (input:int) =
    Math.Abs(input)
    
let manhattan (state)  =
    (abs state.Coords.X) + (abs state.Coords.Y)
    
let solveOne (instructions: (char*int) seq) =
    go initialShipState instructions |> manhattan 

    
let solveTwo (input: string) = input
