module Common

let parseLines (s: string) =
    s.Split [| '\n'; '\r' |]
    |> Seq.map (fun s -> s.Trim())
    |> Seq.filter (fun s -> s <> "")

let parseLineGroups (s: string) =
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
