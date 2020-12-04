module Solve

open System


let parse (input: string) =
    input.Split("\n\n")
    |> Seq.map (fun s -> s.Trim())
    |> Seq.filter (fun s -> s.Length > 0)
    |> Seq.map (fun s ->
        s.Split([| ' '; '\n' |])
        |> Seq.map (fun s2 -> s2.Split(':'))
        |> Seq.map (fun a -> (a.[0], a.[1]))
        |> Array.ofSeq)

let validateHasAllFields (input: (string*string) seq) =
    input 
            |> Seq.map fst
            |> Seq.sort
            |> Seq.filter (fun s -> s <> "cid")
            |> Seq.reduce (sprintf "%s-%s")
            |> (fun s -> String.Equals(s, "byr-ecl-eyr-hcl-hgt-iyr-pid"))
    
let solve (input: string) =
    let parsed = parse input
    parsed
    |> Seq.filter (validateHasAllFields)
    |> Seq.length
