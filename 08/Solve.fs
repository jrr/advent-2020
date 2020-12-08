module Solve

open System

let solve (input: string) = input

let parse (input: string) =
    let splits = input.Split(' ')
    let instr = splits.[0]
    let operand = splits.[1]
    (instr, int operand)
