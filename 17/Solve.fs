module Solve

type Point = int * int

//      maybe if needed for optimization later, but for now using them is harder
//        struct
//                val X: int
//                val Y: int
//        new(x, y) = { X = x; Y = y }
//        end

let parseLine (input: string) (y: int): Point seq =
    input
    |> Seq.mapi (fun x c -> if (c = '#') then Some(x, y) else None)
    |> Seq.choose id

let parse (input: string) =
    input
    |> Common.nonEmptyLines
    |> Seq.mapi (fun i line -> parseLine line i)
    |> Seq.concat
    |> Set.ofSeq

let neighborVectors =
    [| (-1, -1)
       (-1, 0)
       (-1, 1)
       (0, -1) (*0,0*)
       (0, 1)
       (1, -1)
       (1, 0)
       (1, 1) |]

let addPoint p1 p2 = (fst p1 + fst p2), (snd p1 + snd p2)

let neighbors (p: int * int) =
    neighborVectors
    |> Array.map (fun n -> addPoint n p)
    |> Set.ofArray

let solveOne (input: string) = input

let solveTwo (input: string) = input
