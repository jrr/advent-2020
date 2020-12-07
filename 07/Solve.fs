module Solve

open System


type ParsedLine = string * ((int * string) list option)

let parseLine (s: string): ParsedLine =
    let sides = s.Split(" bags contain ")
    let outerColor = sides.[0]

    let innerColors =
        if sides.[1] = "no other bags." then
            None
        else
            sides.[1].Replace(".", "").Split(',')
            |> Seq.map (fun c ->
                let words =
                    c.Replace(" bags", "").Replace(" bag", "").Trim().Split(' ')
                    |> List.ofArray

                let number :: color = words
                (int number, (color |> String.concat " ")))
            |> Seq.toList
            |> Some

    (outerColor, innerColors)

let flattenLine (record: ParsedLine) =
    let left = fst record
    let right = snd record
    match right with
    | None -> []
    | Some r -> r |> (List.map (fun x -> (left, snd x)))

let buildMap input =
    input
    |> Common.nonEmptyLines
    |> Seq.map parseLine
    |> Seq.map flattenLine
    |> Seq.concat
    |> Seq.map (fun (a, b) -> (b, a))
    |> Seq.groupBy fst
    |> Seq.map (fun (innerBag, y) -> (innerBag, (y |> Seq.map snd |> List.ofSeq)))
    |> Map.ofSeq


let rec go (map: Map<string, string list>) color =
    let row = map |> Map.tryFind color
    match row with
    | None -> [ color ]
    | Some colors ->
        let results = colors |> Seq.map (fun c -> go map c)

        let accum =
            color :: (results |> List.concat) |> List.distinct

        accum

let findOtherColorsContaining (map: Map<string, string list>) (color: string) =
    let result = go map color
    result |> List.filter (fun c -> c <> color)
