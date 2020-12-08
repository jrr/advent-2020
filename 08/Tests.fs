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

type TerminationCondition =
    | InfiniteLoop of MachineState
    | EndOfProgram of MachineState
    
let rec go (instructions: list<string * int>) (machineState:MachineState) (trace:int list) : TerminationCondition =
    if trace |> List.contains machineState.iptr then
        InfiniteLoop machineState
    else
        let maybeInstruction = instructions |> List.tryItem machineState.iptr
        match maybeInstruction with
        | None -> EndOfProgram machineState
        | Some instruction ->
            let newState = match instruction with
                                | "acc",x -> {machineState with iptr = (machineState.iptr+1);accum=machineState.accum + x}
                                | "jmp",x -> {machineState with iptr = (machineState.iptr+x)}
                                | "nop",_ -> {machineState with iptr = (machineState.iptr+1)}
                                | _ -> failwith "unrecognized instruction"
            go instructions newState (machineState.iptr::trace)
    
let execute input = 
    let instructions = input |> Common.nonEmptyLines |> Seq.map parse |> Seq.toList
    go instructions initialMachineState []
    
[<Fact>]
let ``solves 8a example`` () =
    execute Input.inputLines |> should equal (InfiniteLoop {iptr=1;accum=5})
    
[<Fact>]
let ``solves 8a problem`` () =
    execute Input.input |> should equal (InfiniteLoop {iptr=359;accum=1137})
    
[<Fact>]
let ``terminates at the end of a program`` () =
    execute Input.terminatingInput |> should equal (EndOfProgram {iptr=9;accum=8})
    
