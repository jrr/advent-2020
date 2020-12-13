module Solve

type Bus =
    | OutOfService
    | BusId of int

type ProblemInput =
    { earliestDeparture: int
      buses: Bus list }

let parseBus (s: string) =
    match s with
    | "x" -> OutOfService
    | n -> BusId(int n)

let parse (input: string) =
    let one :: two :: _ =
        (input |> Common.nonEmptyLines |> Seq.toList)

    let buses =
        two.Split(',')
        |> Array.map parseBus
        |> List.ofArray

    { earliestDeparture = int one
      buses = buses }

let solveOne (input: ProblemInput) =
    let busIds =
        input.buses
        |> Seq.choose (fun b ->
            match b with
            | OutOfService -> None
            | BusId n -> Some n)

    let (busId, departureTime) =
        busIds
        |> Seq.map (fun busId ->
            let roundDown = (input.earliestDeparture / busId)
            let roundDownDeparture = roundDown * busId

            let departure =
                if roundDownDeparture < input.earliestDeparture
                then roundDownDeparture + busId
                else roundDownDeparture

            (busId, departure))
        |> Seq.sortBy snd
        |> Seq.head

    (busId,departureTime)
    let minutesToWait = departureTime - input.earliestDeparture
    minutesToWait * busId

let solveTwo (input: ProblemInput) = input
