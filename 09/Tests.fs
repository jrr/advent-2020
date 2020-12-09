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

let validate (preamble:int seq) (candidate:int) =
    let pairs = Seq.allPairs preamble preamble |> Seq.distinct
    printfn "pairs %O" pairs
    
    match pairs |> Seq.tryFind (fun (a,b) -> a+b = candidate) with
    | None -> false
    | Some _ -> true
    
[<Fact>]
let ``validates good`` ()=
    validate (seq [1;2;3;4;5]) 6 |> should equal true
    
[<Fact>]
let ``validates bad`` ()=
    validate (seq [1;2;3;4;5]) 15 |> should equal false
    
let rec go (nums:int seq) (windowLen:int) =
    let window = nums |> Seq.take windowLen
    let candidate = nums |> Seq.skip windowLen |> Seq.head
    printfn "window %O candidate %O" window candidate
    let valid = validate window candidate
    if valid then
        go (nums |> Seq.skip 1) windowLen
    else
        candidate
    
[<Fact>]
let ``solves example`` ()=
    let intSeq = Input.exampleInput |> Common.nonEmptyLines |> Seq.map int
    let windowLen = 5
    go intSeq windowLen |> should equal 127
    
[<Fact>]
let ``solves problem`` ()=
    let intSeq = Input.problemInput |> Common.nonEmptyLines |> Seq.map int
    let windowLen = 25
    go intSeq windowLen |> should equal 507622668
    
