module Solve

open System
open Common

type Coords = { X: int; Y: int }

let parseLine (s: string) =
    match s with
    | Regex @"(\D)(\d+)" [ letter; num ] -> (letter.[0], int num)
    | _ -> failwith "parse error"

type ShipState = { Facing: int; Coords: Coords }

let initialShipState =
    { Facing = 90
      Coords = { X = 0; Y = 0 } }

let add (a: Coords) (b: Coords) = { X = a.X + b.X; Y = a.Y + b.Y }

let North i = { X = 0; Y = i }
let South i = { X = 0; Y = 0 - i }
let East i = { X = i; Y = 0 }
let West i = { X = 0 - i; Y = 0 }

let moveCoords state c =
    { state with
          Coords = add state.Coords c }

let goForward state i =
    match state.Facing with
    | 0 -> moveCoords state (North i)
    | 90 -> moveCoords state (East i)
    | 180 -> moveCoords state (South i)
    | 270 -> moveCoords state (West i)
    | _ -> failwith "unhandled direction"


let reduceShipState (state: ShipState) (instr: char * int) =
    match instr with
    | ('N', i) -> moveCoords state (North i)
    | ('E', i) -> moveCoords state (East i)
    | ('S', i) -> moveCoords state (South i)
    | ('W', i) -> moveCoords state (West i)
    | ('L', i) ->
        { state with
              Facing = ((state.Facing + 360 - i) % 360) }
    | ('R', i) ->
        { state with
              Facing = ((state.Facing + 360 + i) % 360) }
    | ('F', i) -> (goForward state i)
    | x -> failwith (sprintf "unhandled instruction %O" x)

let rec go (state: ShipState) (instructions: (char * int) seq) =
    let tail = instructions |> Seq.tail

    match instructions |> Seq.tryHead with
    | None -> state
    | Some instr -> go (reduceShipState state instr) tail

let abs (input: int) = Math.Abs(input)

let manhattan (coords) = (abs coords.X) + (abs coords.Y)

let solveOne (instructions: (char * int) seq) =
    go initialShipState instructions
    |> (fun s -> manhattan s.Coords)

type StateTwo = { ShipPos: Coords; Waypoint: Coords }

let initialStateTwo =
    { ShipPos = { X = 0; Y = 0 }
      Waypoint = { X = 10; Y = 1 } }

let quadrant (c: Coords) =
    (if c.X > 0 then 1 else -1), (if c.Y > 0 then 1 else -1)

let rot90 (c: Coords) =
    let q = quadrant c

    let mx, my =
        match q with
        | (1, 1) -> (1, -1)
        | (1, -1) -> (-1, -1)
        | (-1, -1) -> (-1, 1)
        | (-1, 1) -> (1, 1)
        | _ -> failwith "bonk"

    { X = (abs c.Y) * mx
      Y = (abs c.X) * my }


let rec rotateWaypoint (state: StateTwo) (degrees: int) =
    let dir = (degrees + 360) % 360
    match dir with
    | 0 -> state
    | n ->
        rotateWaypoint
            { state with
                  Waypoint = (state.Waypoint |> rot90) }
            (dir - 90)

let mul (a: Coords) (b: int) = { X = a.X * b; Y = a.Y * b }

let moveWaypoint state c =
    { state with
          Waypoint = add state.Waypoint c }

let reduce (state: StateTwo) (instr: Char * int) =
    match instr with
    | 'N', i -> moveWaypoint state (North i)
    | 'S', i -> moveWaypoint state (South i)
    | 'E', i -> moveWaypoint state (East i)
    | 'W', i -> moveWaypoint state (West i)
    | 'R', i -> rotateWaypoint state i
    | 'L', i -> rotateWaypoint state (0 - i)
    | 'F', i ->
        { state with
              ShipPos = add state.ShipPos (mul state.Waypoint i) }
    | _ -> failwith "bonk"


let rec go2 (state: StateTwo) (instructions: (char * int) seq) =
    let tail = instructions |> Seq.tail

    match instructions |> Seq.tryHead with
    | None -> state
    | Some instr -> go2 (reduce state instr) tail



let solveTwo (instructions: (char * int) seq) =
    let endState = go2 initialStateTwo instructions
    endState |> (fun s -> manhattan s.ShipPos)
