module Tests

open Xunit
open FsUnit.Xunit
open Solve


[<Fact>]
let ``reads lines of text`` () =
    Input.exampleInput
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


[<Fact>]
let ``solves 8a example`` () =
    execute Input.exampleInput
    |> should equal (InfiniteLoop({ iptr = 4; accum = 5 }, { iptr = 1; accum = 5 }, ("jmp", -3)))

[<Fact>]
let ``solves 8a problem`` () =
    execute Input.problemInput
    |> should equal (InfiniteLoop({ iptr = 344; accum = 1137 }, { iptr = 359; accum = 1137 }, ("jmp", 15)))

[<Fact>]
let ``terminates at the end of a program`` () =
    execute Input.terminatingInput
    |> should equal (EndOfProgram({ iptr = 8; accum = 2 }, { iptr = 9; accum = 8 }, ("acc", 6)))


[<Fact>]
let ``permutes program`` () =
    let input = [ ("nop", 0); ("acc", 1); ("jmp", 4) ]
    input
    |> permute
    |> Seq.toList
    |> should
        equal
           [ [ ("jmp", 0); ("acc", 1); ("jmp", 4) ]
             [ ("nop", 0); ("acc", 1); ("nop", 4) ] ]

[<Fact>]
let ``solves 8b example`` () =
    let ending = solve8b Input.exampleInput
    printfn "Ending! %O" ending
    let (EndOfProgram (oldState, newState, instr)) = ending
    newState.accum |> should equal 8

[<Fact>]
let ``solves 8b problem`` () =
    let ending = solve8b Input.problemInput
    printfn "Ending! %O" ending
    let (EndOfProgram (oldState, newState, instr)) = ending
    newState.accum |> should equal 1125
