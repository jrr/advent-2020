module Solve

type Point = int*int
        
//      maybe if needed for optimization later, but for now using them is harder
//        struct
//                val X: int
//                val Y: int
//        new(x, y) = { X = x; Y = y }
//        end
        
let parseLine (input:string) (y:int) : Point seq = input |> Seq.mapi (fun x c -> if (c = '#') then Some (x,y) else None) |> Seq.choose id

let parse (input:string) =
        input |> Common.nonEmptyLines |> Seq.mapi (fun i line -> parseLine line i) |> Seq.concat |> Set.ofSeq
        
let solveOne (input: string) = input

let solveTwo (input: string) = input
