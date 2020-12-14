module Solve

open System
open Common


let parse (input: string) =
    input
    |> lineGroups
    |> Seq.map (fun linesInGroup ->
        linesInGroup
        |> String.concat " "
        |> fun s -> s.Split([| ' '; '\n' |])
        |> Seq.map (fun s2 -> s2.Split(':'))
        |> Seq.map (fun a -> (a.[0], a.[1]))
        |> Array.ofSeq)

let validateHasAllFields (input: (string * string) seq) =
    input
    |> Seq.map fst
    |> Seq.sort
    |> Seq.filter (fun s -> s <> "cid")
    |> Seq.reduce (sprintf "%s-%s")
    |> (fun s -> String.Equals(s, "byr-ecl-eyr-hcl-hgt-iyr-pid"))

let yearBetween (s: string) (a: int) (b: int) =
    s.Length = 4 && (int s) >= a && (int s) <= b

let validateHeight (s: string) =
    match s with
    | Regex @"(\d+)cm" [ num ] -> (int num >= 150) && (int num <= 193)
    | Regex @"(\d+)in" [ num ] -> (int num >= 59) && (int num <= 76)
    | _ -> false

let validateFieldValue (input: (string * string)): bool =
    match input with
    | "byr", v -> yearBetween v 1920 2002
    | "iyr", v -> yearBetween v 2010 2020
    | "eyr", v -> yearBetween v 2020 2030
    | "hgt", v -> validateHeight v
    | "hcl", Regex @"#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]" [] -> true
    | "hcl", _ -> false
    | "ecl", v ->
        [ "amb"
          "blu"
          "brn"
          "gry"
          "grn"
          "hzl"
          "oth" ]
        |> Seq.contains v
    | "pid", Regex @"^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$" [] -> true
    | "cid", _ -> true
    | _ -> false

let validateFieldValues (input: (string * string) seq): bool =
    input
    |> Seq.map validateFieldValue
    |> Seq.contains false
    |> not

let solve (input: string) =
    let parsed = parse input
    parsed
    |> Seq.filter (validateHasAllFields)
    |> Seq.length

let solve2 (input: string) =
    let parsed = parse input
    parsed
    |> Seq.filter (validateHasAllFields)
    |> Seq.filter (validateFieldValues)
    |> Seq.length
