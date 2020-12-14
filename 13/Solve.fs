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

let testTimestamp (n: int64) (buses: BusPosition list) =
    buses
    |> Seq.map (fun bus -> (n + (int64 bus.Position)) % (int64 bus.BusId) = 0L)
    |> Seq.reduce (&&)

let solveTwo (input: ProblemInput) =
    let buses = busPositions input

    let largestBusIdBus =
        buses
        |> Seq.sortBy (fun b -> b.BusId)
        |> Seq.rev
        |> Seq.head

    printfn "Largest busId is %d as position %d" largestBusIdBus.BusId largestBusIdBus.Position

    let cheaperSequence =
        // optimization: an infinite sequence of numbers that we know matches for the largest busId
        countBy largestBusIdBus.BusId
        |> Seq.map (fun i -> i - (int64 largestBusIdBus.Position))

    (*
        next try: take all the (busId - position)s, and compute the least common multiple of them.

        LCM(a,b)
            // 6 = 2 * 3
            // 8 = 2 * 2 * 2
            // LCM = 24 (2 * 2 * 2 * 3)
            // ( remove from one set of factors, those the are in the other. then merge the lists)
            (factors of a) (factors of b)
    *)

    let result =
        //                 infiniteCount // counted 1068781 times to get answer 1068781
        cheaperSequence // counted 18115 times to get answer 1068781
        |> Seq.mapi (fun i x ->
            if i % 100000 = 0 then printfn "%d ..." i else ()
            i, x)
        |> Seq.find (fun (_, n) -> testTimestamp n buses)

    printfn "counted %d times to get answer %d" (fst result) (snd result)
    snd result

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