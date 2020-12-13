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

type BusPosition = {Position:int;BusId:int}
let busPositions input =
    input.buses |> List.mapi (fun i a -> (i,a)) |> List.choose(fun (i,x) ->
        match x with
        | OutOfService -> None
        | BusId n -> Some {Position =i;BusId=n})
let infiniteCount = Seq.initInfinite id

let testTimestamp n (buses:BusPosition list) (lowestBusPosition:int) =
    buses |> Seq.map (fun bus -> (n + bus.Position ) % bus.BusId = 0) |> Seq.reduce (&&)
    
let solveTwo (input: ProblemInput) =
    let buses = busPositions input
    let lowestBusPosition = buses |> List.map (fun b ->b.Position) |> List.min
    let result = infiniteCount |> Seq.find (fun n -> testTimestamp n buses lowestBusPosition)
    
    result

