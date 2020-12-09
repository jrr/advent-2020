module Tests

open System
open Xunit
open FsUnit.Xunit
open Solve


[<Fact>]
let ``reads lines of text`` () =
    Input.inputLines
    |> Common.nonEmptyLines
    |> Seq.length
    |> should greaterThan 3

[<Fact>]
let ``reads groups of lines of text`` () =
    Input.inputGroups
    |> Common.lineGroups
    |> Seq.length
    |> should equal 2

let validate (preamble: int64 seq) (candidate: int64) =
    let pairs =
        Seq.allPairs preamble preamble |> Seq.distinct

    match pairs
          |> Seq.tryFind (fun (a, b) -> a + b = candidate) with
    | None -> false
    | Some _ -> true

[<Fact>]
let ``validates good`` () =
    validate (seq [ 1L; 2L; 3L; 4L; 5L ]) 6L
    |> should equal true

[<Fact>]
let ``validates bad`` () =
    validate (seq [ 1L; 2L; 3L; 4L; 5L ]) 15L
    |> should equal false

let rec go (nums: int64 seq) (windowLen: int) =
    let window = nums |> Seq.take windowLen
    let candidate = nums |> Seq.skip windowLen |> Seq.head
    let valid = validate window candidate
    if valid then go (nums |> Seq.skip 1) windowLen else candidate

let parseInts = Common.nonEmptyLines >> Seq.map int64

[<Fact>]
let ``solves example`` () =
    let intSeq = Input.exampleInput |> parseInts
    let windowLen = 5
    go intSeq windowLen |> should equal 127L

[<Fact>]
let ``solves problem`` () =
    let intSeq = Input.problemInput |> parseInts
    let windowLen = 25
    go intSeq windowLen |> should equal 507622668L

type RangeFindResult =
    //    | Nope
    | RangeFound of int64 seq

type RangePointer = { start: int; finish: int; sum: int64 }
//let initialRange = {start=0;finish=0;sum=0L}

let rec findContiguousRangeSumming (sum: int64) (currentRange: RangePointer) (intArray: int64 array) =

    let rangeLen = currentRange.finish - currentRange.start
    let currentSum = currentRange.sum

    match currentSum with
    | x when x = sum ->
        printfn "Found! Range len %d, sum %d" rangeLen currentSum
        Some currentRange
    | x when x < sum ->
        //        printfn "len %d, sum %d -> growing range" rangeLen currentSum
        if currentRange.finish > intArray.Length then
            None
        else
            let newEnd = currentRange.finish + 1
            let newDigit = intArray.[newEnd]

            let newRange2 =
                { currentRange with
                      finish = newEnd
                      sum = currentRange.sum + newDigit }

            findContiguousRangeSumming sum newRange2 intArray
    | x when x > sum ->
        //        printfn "len %d, sum %d -> shrinking range" rangeLen currentSum
        let newStart = currentRange.start + 1
        let removedDigit = intArray.[currentRange.start]

        let newRange2 =
            { currentRange with
                  start = newStart
                  sum = currentRange.sum - removedDigit }

        findContiguousRangeSumming sum newRange2 intArray


//[<Fact>]
//let ``finds 9b example range`` () =
//    let intSeq = Input.exampleInput |> parseInts
//    findContiguousRangeSumming [] intSeq 127L
//    |> should equal (RangeFound(seq [ 15L; 25L; 47L; 40L ]))

let rec findContiguousRangesLessThan nums max =
    seq {
        if Seq.isEmpty nums then
            ()
        else

            let seq1 =
                nums |> Seq.takeWhile (fun n -> n <= max)

            let n = seq1 |> Seq.length
            if n > 0 then
                yield seq1
                let remainder = nums |> Seq.skip n
                yield! findContiguousRangesLessThan remainder max
            else
                let remainder = nums |> Seq.skip 1
                yield! findContiguousRangesLessThan remainder max


    }


let initialRange (intArray: int64 array) =
    { start = 0
      finish = 0
      sum = (intArray.[0]) }

let solve9b input sum =
    let intSeq = input |> parseInts

    let ranges = findContiguousRangesLessThan intSeq sum
    printfn "split the input into %d candidate ranges" (ranges |> Seq.length)

    let intArray = intSeq |> Seq.toArray

    let result =
        findContiguousRangeSumming sum (initialRange intArray) intArray

    result
    |> Option.map (fun r ->
        let foo =
            Array.sub intArray r.start (r.finish - r.start)

        let min = foo |> Seq.min
        let max = foo |> Seq.max
        min + max)

[<Fact>]
let ``solves 9b example`` () =
    solve9b Input.exampleInput 127L
    |> should equal (Some 62L)

[<Fact>]
let ``solves 9b problem`` () =
    solve9b Input.problemInput 507622668L
    |> should equal (Some 76688505L)

[<Fact>]
let ``finds contiguous ranges under a limit`` () =
    findContiguousRangesLessThan [ 1; 2; 5; 3; 4; 10; 11; 3; 1 ] 4
    |> Seq.map (List.ofSeq)
    |> List.ofSeq
    |> should equal [ [ 1; 2 ]; [ 3; 4 ]; [ 3; 1 ] ]
