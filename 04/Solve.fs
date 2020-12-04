module Solve

open System


let parse (input: string) =
    input.Split("\n\n")
    |> Seq.map (fun s -> s.Trim())
    |> Seq.map (fun s ->
        s.Split([| ' '; '\n' |])
        |> Seq.map (fun s2 -> s2.Split(':'))
        |> Seq.map (fun a -> (a.[0], a.[1]))
        |> Array.ofSeq)

let solve (input: string) =
    let parsed = parse input
    parsed
    |> Seq.map (fun r ->
        let keys =
            r
            |> Seq.map fst
            |> Seq.sort
            |> Seq.filter (fun s -> s <> "cid")
            |> Seq.reduce (sprintf "%s-%s")

        String.Equals(keys, "byr-ecl-eyr-hcl-hgt-iyr-pid"))
    |> Seq.filter id
    |> Seq.length
