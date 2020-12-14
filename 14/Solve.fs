module Solve

open System.Net.Sockets
open Common

type Write = { Address: uint; Value: uint64 }
type MaskWithWrite = { Mask: string; Write: Write } // todo: uint64 for addresses too

type ParsedLine =
    | MaskLine of string
    | WriteLine of Write

let parseLine (s: string) =
    //mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
    //mem[8] = 11
    //mem[7] = 101
    //mem[8] = 0
    match s with
    | Regex @"mask = ([01X]*)$" [ mask ] -> MaskLine mask
    | Regex @"mem\[(\d+)\] = (\d+)" [ address; value ] ->
        WriteLine
            { Address = uint address
              Value = uint64 value }
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
    | Some (MaskLine m) -> applyMasksToLinesInner (input |> Seq.tail) m
    | Some (WriteLine w) ->
        (w.Address, (applyMask w.Value currentMask))
        :: (applyMasksToLinesInner (input |> Seq.tail) currentMask)


let applyMasksToLines (input: ParsedLine seq) = applyMasksToLinesInner input "X"

let reduceWrites (input: (uint * uint64) seq) =
    input |> Seq.rev |> Seq.distinctBy fst |> Seq.rev

//let expandAddressesInner (input: ParsedLine seq) =
let expandAddresses (input: ParsedLine seq) =


    [ (0 |> uint, 0UL) ]

let solveOne (input: string) =
    input
    |> parse
    |> applyMasksToLines
    |> reduceWrites
    |> Seq.map snd
    |> Seq.sum

let solveTwo (input: string) =

    input
    |> parse
    |> expandAddresses
    |> reduceWrites
    |> Seq.map snd
    |> Seq.sum
