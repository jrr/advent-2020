module Solve

open System

type Color = string

type InnerBags =
    | NoOtherBags
    | SomeBags of (int * Color) list

type ParsedLine =
    { OuterBagColor: Color
      InnerBags: InnerBags }


let parseLine (s: string): ParsedLine =
    let sides = s.Split(" bags contain ")

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

    let innerBags: InnerBags =
        match innerColors with
        | None -> NoOtherBags
        | Some x -> x |> SomeBags

    { OuterBagColor = sides.[0]
      InnerBags = innerBags }

let flattenLine (record: ParsedLine) =
    let left = record.OuterBagColor
    let right = record.InnerBags
    match right with
    | NoOtherBags -> []
    | SomeBags b -> b |> (List.map (fun x -> (left, snd x)))

let flip (a, b) = (b, a)

/// returns a map of innerBagColor to a list of bag colors that contain it
let buildMapOfEdges input =
    input
    |> Common.nonEmptyLines
    |> Seq.map parseLine
    |> Seq.map flattenLine
    |> Seq.concat
    |> Seq.map flip
    |> Seq.groupBy fst
    |> Seq.map (fun (innerBag, y) -> (innerBag, (y |> Seq.map snd |> List.ofSeq)))
    |> Map.ofSeq


let rec go (map: Map<Color, Color list>) color =
    let row = map |> Map.tryFind color
    match row with
    | None -> [ color ]
    | Some colors ->
        let results = colors |> Seq.map (fun c -> go map c)

        let accum =
            color :: (results |> List.concat) |> List.distinct

        accum

let findOtherColorsContaining (map: Map<Color, Color list>) (color: Color) =
    let result = go map color
    result |> List.filter (fun c -> c <> color)

let rec countContainedBags (data: ParsedLine seq) (color: Color) =
    let x =
        data
        |> Seq.tryFind (fun s -> s.OuterBagColor = color)

    match x with
    | None -> 0
    | Some contained ->
        match contained.InnerBags with
        | NoOtherBags -> 0
        | SomeBags b ->
            b
            |> Seq.map (fun bag ->
                let numBags = fst bag
                let contained = countContainedBags data (snd bag)
                numBags * ( 1 + contained )
            )
            |> Seq.sum
