module Solve

type Point = int * int * int

//      maybe if needed for optimization later, but for now using them is harder
//        struct
//                val X: int
//                val Y: int
//        new(x, y) = { X = x; Y = y }
//        end

let parseLine (input: string) (y: int): Point seq =
    input
    |> Seq.mapi (fun x c -> if (c = '#') then Some(x, y, 0) else None)
    |> Seq.choose id

let parse (input: string) =
    input
    |> Common.nonEmptyLines
    |> Seq.mapi (fun i line -> parseLine line i)
    |> Seq.concat
    |> Set.ofSeq

let neighborVectors : Point seq = [-1;0;1] |> Seq.map (fun x -> [-1;0;1] |> Seq.map (fun y -> [-1;0;1] |> Seq.map (fun z -> x,y,z))) |> Seq.concat |> Seq.concat

let xOf (x, _, _) = x
let yOf (_, y, _) = y
let zOf (_, _, z) = z

let addPoint (p1:Point) (p2:Point) :Point= (xOf p1 + xOf p2), (yOf p1 + yOf p2), (zOf p1 + zOf p2)

let neighbors (p: Point) =
    neighborVectors
    |> Seq.map (fun n -> addPoint n p)
    |> Seq.filter (fun r -> r <> p)
    |> Set.ofSeq

let solveOne (input: string) = input

let solveTwo (input: string) = input
