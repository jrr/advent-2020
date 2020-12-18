module Solve

let parse (input: string) =
    input
    // |> Common.lineGroups
    |> Common.nonEmptyLines

let solveOne (input: string) = input |> parse |> id

let solveTwo (input: string) = input |> parse |> id
