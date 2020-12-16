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

let ticketIsValid (ticket: TicketFieldValues) (allRanges: IntRange seq) =
    ticket
    |> Seq.exists (fun v -> notValidForAnyField allRanges v)
    |> not

let filterInvalidTickets (tickets: TicketFieldValues seq) (allRanges: IntRange seq) =
    tickets
    |> Seq.filter (fun t -> ticketIsValid t allRanges)

let filterOutInvalidTickets (input: Input) =
    let allRanges =
        input.Fields
        |> Seq.collect (fun f -> [ fst f.Range; snd f.Range ])
        |> Seq.sort

    { input with
          NearbyTickets =
              (filterInvalidTickets (input.NearbyTickets |> List.ofSeq) allRanges
               |> List.ofSeq) }

let colsFromRows (a: int list list) =
    let numCols = a.[0].Length
    { 0 .. numCols - 1 }
    |> Seq.map (fun colNum -> a |> Seq.map (fun row -> row.[colNum]))
    |> Seq.map Seq.toList
    |> Seq.toList

let validForAllFields (ranges: IntRange seq) (value:int) =
    ranges |> Seq.forall (fun r -> withinRange r value)
    
let matchingFieldsForCol col (fields:TicketField list) =
    fields |> Seq.filter (fun field -> col |> Seq.forall (fun c -> (withinRange (fst field.Range) c) || (withinRange (snd field.Range) c)))
    
let solveTwo (input: Input): (string * int) list =
    let input2 = input |> filterOutInvalidTickets
    let cols = colsFromRows input2.NearbyTickets
//    let fieldNames = input.Fields |> Seq.map (fun f -> f.Name)

    let matches = cols |> Seq.mapi (fun i col ->
        let result = matchingFieldsForCol col input2.Fields
        i,result
        )
    
    matches  |> Seq.iter (fun (i,names) ->
        let fieldNames = names |> Seq.map (fun r -> r.Name) |> Seq.reduce (sprintf "%s,%s")
        printfn "column %d could be %d fields: %s" i (names |> Seq.length) fieldNames
        )
    []
