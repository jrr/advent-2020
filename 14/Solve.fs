module Solve

open Common

type ParsedLine =
    | Mask of string
    | Write of uint * uint64

let parseLine (s: string) =
    //mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
    //mem[8] = 11
    //mem[7] = 101
    //mem[8] = 0
    match s with
    | Regex @"mask = ([01X]*)$" [ mask ] -> Mask mask
    | Regex @"mem\[(\d+)\] = (\d+)" [ address; value ] -> Write(uint address, uint64 value)
    | x -> failwith $"parse error ({x})"

let parse (input: string) =
    input |> Common.nonEmptyLines |> Seq.map parseLine

open System

let parseBinary (input: string) = Convert.ToUInt64(input, 2)

let applyMask (value: uint64) (mask: string) =
    let orMask = mask.Replace('X', '0') |> parseBinary
    let andMask = mask.Replace('X', '1') |> parseBinary
    (value &&& andMask) ||| orMask

let rec applyMasksToLinesInner (input: ParsedLine seq) (currentMask: string) =
    let head = input |> Seq.tryHead
    match head with
    | None -> []
    | Some (Mask m) -> applyMasksToLinesInner (input |> Seq.tail) m
    | Some (Write (a, b)) ->
        (a, (applyMask b currentMask))
        :: (applyMasksToLinesInner (input |> Seq.tail) currentMask)


let applyMasksToLines (input: ParsedLine seq) = applyMasksToLinesInner input "X"

let reduceWrites (input: (uint * uint64) seq) =
    input |> Seq.rev |> Seq.distinctBy fst |> Seq.rev

let solveOne (input: string) =
    input
    |> parse
    |> applyMasksToLines
    |> reduceWrites
    |> Seq.map snd
    |> Seq.sum

let solveTwo (input: string) = input
