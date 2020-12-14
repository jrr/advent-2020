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

    (busId, departureTime)
    let minutesToWait = departureTime - input.earliestDeparture
    minutesToWait * busId

type BusPosition = { Position: int; BusId: int }

let busPositions input =
    input.buses
    |> List.mapi (fun i a -> (i, a))
    |> List.choose (fun (i, x) ->
        match x with
        | OutOfService -> None
        | BusId n -> Some { Position = i; BusId = n })

let infiniteCount = Seq.initInfinite id |> Seq.map int64

let countBy n =
    Seq.initInfinite (fun i -> (int64 n) * (int64 i))
    
let countFromBy a b =
    Seq.initInfinite (fun i -> (int64 a) + (int64 i)*(int64 b))

let testTimestamp (n: int64) (buses: BusPosition list) =
    buses
    |> Seq.map (fun bus -> (n + (int64 bus.Position)) % (int64 bus.BusId) = 0L)
    |> Seq.reduce (&&)

let rec search (buses:BusPosition list) (start:int64) (interval:int64) =
    if buses |> List.isEmpty then
        start
    else
        let bus::tail = buses
        let result = countFromBy start interval |> Seq.find (fun i -> (i + (int64 bus.Position)) % (int64 bus.BusId) = 0L)
        
        let newInterval = interval * (int64 bus.BusId)
        printfn "found time for bus %i. interval %i * %i = %i" bus.BusId interval bus.BusId newInterval
        search tail result newInterval
    
let solveTwo (input: ProblemInput) =
    let buses = busPositions input
    
    search buses 0L 1L


let rec innerFactor (n: int) (i: int) =
    if i > n / 2 then [ n ]
    else if n % i = 0 then i :: innerFactor (n / i) i
    else innerFactor n (i + 1)


let factor (n: int) = innerFactor n 2


let mergeFactors (a:int seq) (b:int seq) =
    let allDigits = (Seq.append a b) |> Seq.distinct
    allDigits |> Seq.map (fun i ->
        let countA = a |> Seq.filter(fun x -> x = i)  |> Seq.length
        let countB = b |> Seq.filter(fun x -> x = i) |> Seq.length
        Seq.replicate (max countA countB) i
        ) |> Seq.concat
    
let leastCommonMultiple (a:int) (b:int) =
    let factorsA = factor a
    let factorsB = factor b
    mergeFactors factorsA factorsB |> Seq.reduce (*)