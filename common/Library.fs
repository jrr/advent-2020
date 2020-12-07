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
