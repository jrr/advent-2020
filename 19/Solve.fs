module Solve
open System.Text.RegularExpressions

let parse (input: string) =
    input
    // |> Common.lineGroups
    |> Common.nonEmptyLines

let replaceNumWithString (input:string) (num:int) (replacement:string) =
    Regex.Replace(input ,$"([^\d]){num}([^^\d:])",$"$1{replacement}$2")
    
let solveOne (input: string) = input |> parse |> id

let solveTwo (input: string) = input |> parse |> id
