module Solve

open System.Net.Sockets
open Common

type Write = { Address: uint64; Value: uint64 }
type MaskWithWrite = { Mask: string; Write: Write }

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
            { Address = uint64 address
              Value = uint64 value }
    | x -> failwith $"parse error ({x})"

let parse (input: string) =
    input |> Common.nonEmptyLines |> Seq.map parseLine

open System

let parseBinary (input: string) = Convert.ToUInt64(input, 2)

let printBinary (value:uint64) =
    let range = seq {0..63} |> Seq.rev
    range |> Seq.map (fun n -> if (1UL <<< n) &&& value = 0UL then '0' else '1') |> String.Concat



let rec distributeMasksRec (input: ParsedLine seq) (currentMask: string) =
    let head = input |> Seq.tryHead
    match head with
    | None -> []
    | Some (MaskLine m) -> distributeMasksRec (input |> Seq.tail) m
    | Some (WriteLine w) ->
        {Mask = currentMask;Write = w} :: (distributeMasksRec (input |> Seq.tail) currentMask)
let distributeMasks (input: ParsedLine seq) =
    distributeMasksRec input "X"


let applyValueMaskInner (value: uint64) (mask: string) =
    let orMask = mask.Replace('X', '0') |> parseBinary
    let andMask = mask.Replace('X', '1') |> parseBinary
    (value &&& andMask) ||| orMask
    
let applyValueMask (input: MaskWithWrite ) =
    let masked = applyValueMaskInner input.Write.Value input.Mask
    {input.Write with Value = masked}

let applyValueMasks (input:MaskWithWrite seq) = input |> Seq.map applyValueMask

let reduceWrites (input: Write seq) =
    input |> Seq.rev |> Seq.distinctBy (fun w -> w.Address)|> Seq.rev

let rec applyFloaters (value:uint64) (floaters:int list) =
    match floaters |> List.tryHead with
    | None -> [value]
    | Some bit ->
        let onMask = 1UL<<<bit
        let offMask = ~~~ onMask
        
        let left = (value ||| onMask)
        let right = (value &&& offMask)
        let what = [left;right] |> List.map (fun v ->
            let tail = floaters |> List.tail
            applyFloaters v tail
            )
        what |> List.concat
        
let applyAddressMask (input:MaskWithWrite) : Write seq=
    let orMask = input.Mask.Replace('X','0') |> parseBinary
    let applied = input.Write.Address ||| orMask
    let floatingBits = input.Mask
                       |> Seq.rev
                       |> Seq.mapi (fun i c -> i,c)
                       |> Seq.filter(fun (_,c) ->
                            c = 'X'
                            )
                       |> Seq.map fst |> List.ofSeq
    let result = applyFloaters applied floatingBits
    result |> Seq.map (fun f -> {Address=f;Value=input.Write.Value}) |> Seq.sortBy (fun a -> a.Address)
    
let applyAddressMasks (input:MaskWithWrite seq) : Write seq=
    input |> Seq.map applyAddressMask |> Seq.concat

let solveOne (input: string) =
    input
    |> parse
    |> distributeMasks
    |> applyValueMasks
    |> reduceWrites
    |> Seq.map (fun w -> w.Value)
    |> Seq.sum

let solveTwo (input: string) =

    input
    |> parse
    |> distributeMasks
    |> applyAddressMasks
    |> reduceWrites
    |> Seq.map (fun w -> w.Value)
    |> Seq.sum
