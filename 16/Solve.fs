module Solve

open Common
open System

type TicketFieldValues = int list
type IntRange = int * int

type TicketField =
    { Name: string
      Range: IntRange * IntRange }

type Input =
    { Fields: TicketField list
      MyTicket: TicketFieldValues
      NearbyTickets: TicketFieldValues list }

let parseField =
    function
    | Regex @"([a-z ]+): (\d+)-(\d+) or (\d+)-(\d+)$" [ name; a; b; c; d ] ->
        { Name = name
          Range = ((int a, int b), (int c, int d)) }
    | x -> failwith $"parse error ({x})"

let parseTicketFieldValues (s: string) = s.Split(',') |> Seq.map int

let parse (input: string) =
    let [ rangesLines; myTicketLines; nearbyTicketLines ] = input |> Common.lineGroups |> Seq.toList
    let fields = rangesLines |> Seq.map parseField

    let myTicketValues =
        myTicketLines
        |> Seq.skip 1
        |> Seq.map parseTicketFieldValues
        |> Seq.head

    let nearbyTicketValues =
        nearbyTicketLines
        |> Seq.skip 1
        |> Seq.map parseTicketFieldValues

    { Fields = fields |> Seq.toList
      MyTicket = myTicketValues |> List.ofSeq
      NearbyTickets =
          nearbyTicketValues
          |> Seq.map List.ofSeq
          |> List.ofSeq }


let withinRange (range) (value): bool =
    (value >= fst range) && (value <= snd range)

let notValidForAnyField (ranges: IntRange seq) (value: int) =
    let validForAtLeastOne =
        ranges
        |> Seq.exists (fun r -> withinRange r value)

    not validForAtLeastOne

let solveOne (input: Input) =
    let allRanges =
        input.Fields
        |> Seq.collect (fun f -> [ fst f.Range; snd f.Range ])
        |> Seq.sort

    let allValues =
        input.NearbyTickets |> Seq.collect id |> Seq.sort

    let invalids =
        allValues
        |> Seq.filter (fun v -> notValidForAnyField allRanges v)
    //    input
    invalids |> Seq.reduce (+)

let solveTwo (input: string) = input
