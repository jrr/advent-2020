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
let infiniteCount = Seq.initInfinite id |> Seq.map int64

let countBy n = Seq.initInfinite (fun i -> (int64 n) * (int64 i))

let testTimestamp (n:int64) (buses:BusPosition list)=
    buses |> Seq.map (fun bus -> (n + (int64 bus.Position) ) % (int64 bus.BusId) = 0L) |> Seq.reduce (&&)
    
let solveTwo (input: ProblemInput) =
    let buses = busPositions input
    let largestBusId = buses |> Seq.map (fun b -> b.BusId) |> Seq.max
    
    let result =
                 infiniteCount
//                 countBy largestBusId
                 |> Seq.mapi (fun i x ->
                     if i % 100000 = 0 then printfn "%d ..." i else ()
                     x )
//                 |> Seq.map (fun i -> i - large)
                 |> Seq.find (fun n -> testTimestamp n buses)
    
    result

