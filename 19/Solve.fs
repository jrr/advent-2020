module Solve

open System
open System.Text.RegularExpressions
open Common

type NumOrString =
    | Num of int
    | String of string
    
type Rule =
    | StringRule of string
    | Digits of NumOrString list
    | Or of (NumOrString list) * (NumOrString list)

type ProblemInput =
    { rules: (int * Rule) list
      messages: string list }

let parseLineOfInts (s: string) =
    printfn "parse ints %s" s
    s.Split(" ") |>Seq.filter (fun x -> x <> "")|> Seq.map int |> Seq.map Num |>Seq.toList

let parseOrLine (s:string) =
    let splits = s.Split("|")
    (parseLineOfInts splits.[0]),(parseLineOfInts splits.[1])

let parseRule (input: string) =
    match input with
    | Regex @"^(\d+): ([\d ]+)$" [ i; s ] -> (int i, (Digits(parseLineOfInts s)))
    | Regex @"^(\d+): ([\d \|]+)$" [ i; s ] -> (int i, (Or (parseOrLine s)))
    | Regex """^(\d+): "([ab])"$""" [ i; s ] -> (int i, StringRule s)
    | z -> failwith (sprintf "unrecognized input %s" z)

let parse (input: string) =
    let groups = input |> Common.lineGroups |> Seq.toList
    let rules = groups.[0] |> Seq.map parseRule |> List.ofSeq
    let messages = groups.[1] |> Seq.toList
    {rules = rules;messages = messages}

let replaceNumWithString (input: string) (num: int) (replacement: string) =
    Regex.Replace(input, $"([^\d]){num}([^^\d:])", $"$1{replacement}$2")

let solveOne (input: string) = input |> parse |> id

let solveTwo (input: string) = input |> parse |> id
