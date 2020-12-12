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
    
let manhattan (coords) = (abs coords.X) + (abs coords.Y)
    
let solveOne (instructions: (char*int) seq) =
    go initialShipState instructions |> (fun s -> manhattan s.Coords)

type StateTwo = {ShipPos: Coords;Waypoint:Coords}
let initialStateTwo = {ShipPos={X=0;Y=0};Waypoint={X=10;Y=1}}

let quadrant (c:Coords) =
    (if c.X > 0 then 1 else -1),(if c.Y > 0 then 1 else -1)
    
let rot90 (c:Coords) =
    let q = quadrant c
    let mx,my = match q with
    | (1,1) -> (1,-1)
    | (1,-1) -> (-1,-1)
    | (-1,-1) -> (-1,1)
    | (-1,1) -> (1,1)
    {X=(abs c.Y) * mx;Y=(abs c.X) * my}
    
    
let rotateWaypoint (state:StateTwo) (degrees:int) =
    let dir = (degrees+360) % 360
    match dir with
    | 0 -> state
    | 90 ->  {state with Waypoint= (state.Waypoint|> rot90)}
    | 180 ->  {state with Waypoint= (state.Waypoint|> rot90|>rot90)}
    | 270 ->  {state with Waypoint= (state.Waypoint|> rot90|>rot90|>rot90)}
    
let mul (a:Coords) (b:int) = {X=a.X*b;Y=a.Y*b}

let reduce (state:StateTwo) (instr:Char*int) =
    match instr with
    | 'N',i -> {state with Waypoint= add state.Waypoint {X=0;Y=i}}
    | 'S',i -> {state with Waypoint= add state.Waypoint {X=0;Y=0-i}}
    | 'E',i -> {state with Waypoint= add state.Waypoint {X=i;Y=0}}
    | 'W',i -> {state with Waypoint= add state.Waypoint {X=0-i;Y=0}}
    | 'R',i -> rotateWaypoint state i
    | 'L',i -> rotateWaypoint state (0-i)
    | 'F',i -> {state with ShipPos = add state.ShipPos (mul state.Waypoint i)}
    | _-> failwith "bonk"
    
    
let rec go2 (state:StateTwo) (instructions: (char*int) seq) =
    let tail = instructions |> Seq.tail
    
    match instructions |> Seq.tryHead with
    | None -> state
    | Some instr-> go2 (reduce state instr) tail
    
    
    
let solveTwo (instructions: (char*int) seq) = 
    let endState = go2 initialStateTwo instructions
    endState |> (fun s -> manhattan s.ShipPos)
