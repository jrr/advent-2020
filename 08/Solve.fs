module Solve

let parse (input: string) =
    let splits = input.Split(' ')
    let instr = splits.[0]
    let operand = splits.[1]
    (instr, int operand)


type MachineState = { iptr: int; accum: int }
let initialMachineState: MachineState = { iptr = 0; accum = 0 }

type Instruction = string * int

let maybePatch (i: Instruction): Instruction =
    match i with
    | "nop", x -> "jmp", x
    | "jmp", x -> "nop", x
    | "acc", _ -> i
    | _ -> failwith "unrecognized instruction"

let permute (input: Instruction list): Instruction list seq =
    let numbered = input |> List.mapi (fun i x -> (i, x))
    seq {
        for item in numbered do
            let (_, (instr, _)) = item
            if instr = "acc" then
                ()
            else
                let newList =
                    numbered
                    |> List.map (fun (i, x) -> if i = fst item then maybePatch x else x)

                yield newList
    }

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

let solve8b inputString =
    let permutations =
        inputString
        |> Common.nonEmptyLines
        |> Seq.map parse
        |> Seq.toList
        |> permute

    let endStates =
        permutations
        |> Seq.map (fun program -> go program initialMachineState [])

    let ending =
        endStates
        |> Seq.find (fun s ->
            match s with
            | InfiniteLoop _ -> false
            | EndOfProgram _ -> true)

    ending
