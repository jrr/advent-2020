module Solve

open Common

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

let validForAllFields (ranges: IntRange seq) (value: int) =
    ranges
    |> Seq.forall (fun r -> withinRange r value)


type NumberedCol = { ColNum: int; ColValues: int list }

type ProblemTwo =
    { Columns: NumberedCol list
      CandidateFields: TicketField list }

type SolvedColumn =
    { Column: NumberedCol
      PairedField: TicketField }

let matchingFieldsForCol (col: NumberedCol) (fields: TicketField list) =
    fields
    |> Seq.filter (fun field ->
        col.ColValues
        |> Seq.forall (fun c ->
            (withinRange (fst field.Range) c)
            || (withinRange (snd field.Range) c)))

let rec solveProblem problem: SolvedColumn seq =

    if problem.Columns |> Seq.isEmpty then
        seq []
    else
        let matches =
            problem.Columns
            |> Seq.map (fun col ->
                let result =
                    matchingFieldsForCol col problem.CandidateFields

                col.ColNum, result)

        let solvedOnes =
            matches
            |> Seq.filter (fun (colNum, matchingCols) -> matchingCols |> Seq.length = 1)

        if solvedOnes |> Seq.length > 1
        then printfn ">1 solved column in a single pass! wasn't expecting that."
        else ()


        let solutions =
            solvedOnes
            |> Seq.map (fun (colNum, fields) ->
                let theCol =
                    problem.Columns
                    |> List.find (fun c -> c.ColNum = colNum)

                let theField = fields |> Seq.exactlyOne
                { Column = theCol
                  PairedField = theField })

        let removedColNums =
            solutions |> Seq.map (fun s -> s.Column.ColNum)

        let removedFieldNames =
            solutions |> Seq.map (fun f -> f.PairedField.Name)

        let reducedProblem =
            { problem with
                  Columns =
                      problem.Columns
                      |> List.filter (fun c -> removedColNums |> Seq.contains c.ColNum |> not)
                  CandidateFields =
                      problem.CandidateFields
                      |> List.filter (fun f -> removedFieldNames |> Seq.contains f.Name |> not) }

        let remainder = (solveProblem reducedProblem)
        Seq.append solutions remainder

let solveTwo (input: Input): (string * int) list =
    let input2 = input |> filterOutInvalidTickets
    let cols = colsFromRows input2.NearbyTickets

    let numberedColumns =
        cols
        |> Seq.mapi (fun i col -> { ColNum = i; ColValues = col })

    let problem =
        { Columns = numberedColumns |> Seq.toList
          CandidateFields = input.Fields }

    let solution = solveProblem problem
    solution
    |> Seq.map (fun s -> s.PairedField.Name, input.MyTicket.Item s.Column.ColNum)
    |> List.ofSeq

let myTicketProduct (input: (string * int) list) =
    input
    |> Seq.filter (fun (n, i) -> n.StartsWith "departure")
    |> Seq.map (snd)
    |> Seq.map int64
    |> Seq.reduce (*)
