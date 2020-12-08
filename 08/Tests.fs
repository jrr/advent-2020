module Tests

open System
open Xunit
open FsUnit.Xunit
open Solve

[<Fact>]
let ``solves problem`` () = solve "foo" |> should equal "foo"



[<Fact>]
let ``reads lines of text`` () =
    Input.inputLines
    |> Common.nonEmptyLines
    |> Seq.map parse
    |> Seq.toList
    |> should
        equal
           [ ("nop", 0)
             ("acc", 1)
             ("jmp", 4)
             ("acc", 3)
             ("jmp", -3)
             ("acc", -99)
             ("acc", 1)
             ("jmp", -4)
             ("acc", 6) ]

type MachineState = {iptr : int; accum:int}
let initialMachineState : MachineState = {iptr=0;accum=0}

let rec go (instructions: list<string * int>) (machineState:MachineState) (trace:int list) =
    if trace |> List.contains machineState.iptr then
        failwith (sprintf "infinite loop detected. accum=%d" machineState.accum)
    
    let instruction = instructions.[machineState.iptr]
    let newState = match instruction with
                        | "acc",x -> {machineState with iptr = (machineState.iptr+1);accum=machineState.accum + x}
                        | "jmp",x -> {machineState with iptr = (machineState.iptr+x)}
                        | "nop",_ -> {machineState with iptr = (machineState.iptr+1)}
                        | _ -> failwith "unrecognized instruction"
    go instructions newState (machineState.iptr::trace)
    ()
    
[<Fact>]
let ``solves 8a example`` () =
    let instructions = Input.inputLines |> Common.nonEmptyLines |> Seq.map parse |> Seq.toList
    go instructions initialMachineState []
    // 5
    ()
    
[<Fact>]
let ``solves 8a problem`` () =
    let instructions = Input.input |> Common.nonEmptyLines |> Seq.map parse |> Seq.toList
    go instructions initialMachineState []
    ()
    
