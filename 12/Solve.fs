module Solve

open System

let solve (input: string) = input


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

type ShipState = { (* 0 is north, 90 is east *)Facing: int; X: int; Y: int }

let initialShipState = {Facing=90;X=0;Y=0}

let goForward state i =
    match state.Facing with
    | 0 -> {state with Y = state.Y + i}
    | 90 -> {state with X = state.X + i}
    | 180 -> {state with Y = state.Y - i}
    | 270 -> {state with X = state.X - i}
    
    
let rec go (state:ShipState) (instructions: (char*int) seq) =
    let tail = instructions |> Seq.tail
    
    match instructions |> Seq.tryHead with
    | None -> state
    | Some ('N',i) -> go {state with Y = state.Y + i} tail
    | Some ('E',i) -> go {state with X = state.X + i} tail
    | Some ('S',i) -> go {state with Y = state.Y - i} tail
    | Some ('W',i) -> go {state with X = state.X - i} tail
    | Some ('L',i) -> go {state with Facing = ((state.Facing + 360 - i) % 360) } tail
    | Some ('R',i) -> go {state with Facing = ((state.Facing + 360 + i) % 360) } tail
    | Some ('F',i) -> go (goForward state i) tail
    | instr -> failwith (sprintf "unhandled instruction %O" instr)
    
    
let abs (input:int) =
    Math.Abs(input)
    
let manhattan (state)  =
    (abs state.X) + (abs state.Y)
    
let solveOne (instructions: (char*int) seq) =
    go initialShipState instructions |> manhattan 

    
let solveTwo (input: string) = input
