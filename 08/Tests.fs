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

type MachineState = { iptr: int; accum: int }
let initialMachineState: MachineState = { iptr = 0; accum = 0 }

type Instruction = string * int

type TerminationCondition =
    | InfiniteLoop of MachineState * MachineState * Instruction
    | EndOfProgram of MachineState * MachineState * Instruction

let rec go (instructions: list<string * int>) (machineState: MachineState) (trace: int list): TerminationCondition =
    let instruction = instructions.[machineState.iptr]

    let newState =
        match instruction with
        | "acc", x ->
            { machineState with
                  iptr = (machineState.iptr + 1)
                  accum = machineState.accum + x }
        | "jmp", x ->
            { machineState with
                  iptr = (machineState.iptr + x) }
        | "nop", _ ->
            { machineState with
                  iptr = (machineState.iptr + 1) }
        | _ -> failwith "unrecognized instruction"

    if instructions
       |> List.tryItem newState.iptr
       |> Option.isNone then
        EndOfProgram(machineState, newState, instruction)
    else if trace |> List.contains newState.iptr then
        InfiniteLoop(machineState, newState, instruction)
    else
        go instructions newState (machineState.iptr :: trace)

let execute input =
    let instructions =
        input
        |> Common.nonEmptyLines
        |> Seq.map parse
        |> Seq.toList

    go instructions initialMachineState []

[<Fact>]
let ``solves 8a example`` () =
    execute Input.inputLines
    |> should equal (InfiniteLoop({ iptr = 4; accum = 5 }, { iptr = 1; accum = 5 }, ("jmp", -3)))

[<Fact>]
let ``solves 8a problem`` () =
    execute Input.input
    |> should equal (InfiniteLoop({ iptr = 344; accum = 1137 }, { iptr = 359; accum = 1137 }, ("jmp", 15)))

[<Fact>]
let ``terminates at the end of a program`` () =
    execute Input.terminatingInput
    |> should equal (EndOfProgram({ iptr = 8; accum = 2 }, { iptr = 9; accum = 8 }, ("acc", 6)))
