module Tests

open System
open Xunit
open FsUnit.Xunit
open Solve

[<Fact>]
let ``solves problem`` () = solve "foo" |> should equal "foo"



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

let solve10a (input: int seq) =
    let sorted = input |> Seq.sort
    let max = sorted |> Seq.max
    let joltDifferences = sorted
                                |> Seq.append [ 0; max + 3]
                                |> Seq.sort
                                |> Seq.pairwise
                                |> Seq.map (fun (a, b) -> b - a)
                                |> Seq.countBy id
        //    |> Seq.map (fun (count,value)-> 0)
    let oneJoltDifferences = joltDifferences |> Seq.find (fun (a,_) -> a = 1) |> snd
    let threeJoltDifferences = joltDifferences |> Seq.find (fun (a,_) -> a = 3) |> snd
    oneJoltDifferences * threeJoltDifferences
    
[<Fact>]
let ``solves 10a example`` () =
    solve10a Input.exampleInput  |> should equal 35
    ()
    
[<Fact>]
let ``solves 10a example2`` () =
    solve10a Input.example2 |> should equal 220
    ()
    
[<Fact>]
let ``solves 10a problem`` () =
    solve10a Input.problemInput |> should equal 2312
    ()
