module Solve

open System

let solve (input: string) = input

let parseBinary s = Convert.ToInt32(s, 2)

let parse (input: string) =
    let left =
        input.Substring(0, 7).Replace('B', '1').Replace('F', '0')
        |> parseBinary

    let right =
        input.Substring(7).Replace('R', '1').Replace('L', '0')
        |> parseBinary

    (left, right)

let seatId (x: int * int) =
    let (l, r) = x
    l * 8 + r

let charToBit (c:char) =
    match c with
    | 'B' -> '1'
    | 'F' -> '0'
    | 'R' -> '1'
    | 'L' -> '0'
    | _ -> failwith "unrecognized char"

let calc (s:string) =
    s |> Seq.map(charToBit) |> String.Concat |> parseBinary
