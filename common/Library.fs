module Common


let lineSeq (s: string) =
    let splits = s.Split [| '\n'; '\r' |]
    seq {
        for line in splits do
            yield line
    }


let nonEmptyLines (s: string) =
    s
    |> lineSeq
    |> Seq.map (fun s -> s.Trim())
    |> Seq.filter (fun s -> s.Length > 0)

let lineGroups (s: string) = // todo: build in terms of above functions
    s.Replace("\r", "").Split("\n\n")
    |> Seq.map (fun s -> s.Trim())
    |> Seq.filter (fun s -> s <> "")
    |> Seq.map (fun g ->
        let x =
            g.Split('\n')
            |> Seq.map (fun l -> l.Trim())
            |> Seq.filter (fun l -> l <> "")

        x)
    |> Seq.filter (fun g -> g |> Seq.length > 0)

open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

//    //Example:
//    let phone = "(555) 555-5555"
//    match phone with
//    | Regex @"\(([0-9]{3})\)[-. ]?([0-9]{3})[-. ]?([0-9]{4})" [ area; prefix; suffix ] ->
//        printfn "Area: %s, Prefix: %s, Suffix: %s" area prefix suffix
//    | _ -> printfn "Not a phone number"
