module Solve

open System

let solve (input: string) = input

let uniqueCharsInLineGroup: seq<string> -> int =
    String.Concat
    >> Seq.sort
    >> Seq.distinct
    >> Seq.length

let solve6a s =
    s
    |> Common.lineGroups
    |> Seq.map uniqueCharsInLineGroup
    |> Seq.reduce (+)
