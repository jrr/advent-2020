module Solve

open Common
open System

type Write = { Address: uint64; Value: uint64 }
type MaskWithWrite = { Mask: string; Write: Write }

type ParsedLine =
    | MaskLine of string
    | WriteLine of Write

let parseLine =
    function
    | Regex @"mask = ([01X]*)$" [ mask ] -> MaskLine mask
    | Regex @"mem\[(\d+)\] = (\d+)" [ address; value ] ->
        WriteLine
            { Address = uint64 address
              Value = uint64 value }
    | x -> failwith $"parse error ({x})"

let parse (input: string) =
    input |> Common.nonEmptyLines |> Seq.map parseLine

let parseBinary (input: string) = Convert.ToUInt64(input, 2)

let printBinary (value: uint64) =
    seq { 0 .. 63 }
    |> Seq.rev
    |> Seq.map (fun n -> if (1UL <<< n) &&& value = 0UL then '0' else '1')
    |> String.Concat

let rec distributeMasksRec (input: ParsedLine seq) (currentMask: string) =
    match input |> Seq.tryHead with
    | None -> []
    | Some (MaskLine m) -> distributeMasksRec (input |> Seq.tail) m
    | Some (WriteLine w) ->
        { Mask = currentMask; Write = w }
        :: (distributeMasksRec (input |> Seq.tail) currentMask)

let distributeMasks (input: ParsedLine seq) = distributeMasksRec input "X"

let applyValueMaskInner (value: uint64) (mask: string) =
    let orMask = mask.Replace('X', '0') |> parseBinary
    let andMask = mask.Replace('X', '1') |> parseBinary
    (value &&& andMask) ||| orMask

let applyValueMask (input: MaskWithWrite) =
    let masked =
        applyValueMaskInner input.Write.Value input.Mask

    [ { input.Write with Value = masked } ]


let reduceWrites (input: Write seq) =
    input
    |> Seq.rev
    |> Seq.distinctBy (fun w -> w.Address)
    |> Seq.rev

let rec applyFloaters (value: uint64) (floaters: int list) =
    match floaters |> List.tryHead with
    | None -> [ value ]
    | Some bit ->
        let onMask = 1UL <<< bit
        let offMask = ~~~onMask

        [ (value ||| onMask)
          (value &&& offMask) ]
        |> List.map (fun v ->
            let tail = floaters |> List.tail
            applyFloaters v tail)
        |> List.concat

let applyAddressMask (input: MaskWithWrite): Write seq =
    let orMask =
        input.Mask.Replace('X', '0') |> parseBinary

    let applied = input.Write.Address ||| orMask

    let floatingBits =
        input.Mask
        |> Seq.rev
        |> Seq.mapi (fun i c -> i, c)
        |> Seq.filter (fun (_, c) -> c = 'X')
        |> Seq.map fst
        |> List.ofSeq

    applyFloaters applied floatingBits
    |> Seq.sort
    |> Seq.map (fun f ->
        { Address = f
          Value = input.Write.Value })

let solveOne (input: string) =
    input
    |> parse
    |> distributeMasks
    |> Seq.map applyValueMask
    |> Seq.concat
    |> reduceWrites
    |> Seq.sumBy (fun w -> w.Value)

let solveTwo (input: string) =
    input
    |> parse
    |> distributeMasks
    |> Seq.map applyAddressMask
    |> Seq.concat
    |> reduceWrites
    |> Seq.sumBy (fun w -> w.Value)
