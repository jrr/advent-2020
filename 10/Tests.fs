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

    let joltDifferences =
        sorted
        |> Seq.append [ 0; max + 3 ]
        |> Seq.sort
        |> Seq.pairwise
        |> Seq.map (fun (a, b) -> b - a)
        |> Seq.countBy id
    //    |> Seq.map (fun (count,value)-> 0)
    let oneJoltDifferences =
        joltDifferences
        |> Seq.find (fun (a, _) -> a = 1)
        |> snd

    let threeJoltDifferences =
        joltDifferences
        |> Seq.find (fun (a, _) -> a = 3)
        |> snd

    oneJoltDifferences * threeJoltDifferences

[<Fact>]
let ``solves 10a example`` () =
    solve10a Input.exampleInput |> should equal 35
    ()

[<Fact>]
let ``solves 10a example2`` () =
    solve10a Input.example2 |> should equal 220
    ()

[<Fact>]
let ``solves 10a problem`` () =
    solve10a Input.problemInput |> should equal 2312
    ()

let splitGraph (input: int seq) =
    let seed: int seq seq = Seq.empty

    let what =
        input
        |> Seq.fold (fun accum value ->
            match accum |> Seq.tryHead |> Option.bind Seq.tryHead with
            | None -> seq [ seq [ value ] ]
            | Some last ->
                if value - last < 3 then
                    // attach it to previous segment
                    let head :: tail = (accum |> Seq.toList)
                    let newHead = Seq.append [ value ] head
                    newHead :: tail |> Seq.ofList
                else
                    // start a new segment
                    Seq.append [ [ value ] ] accum

            ) seed

    what
    |> Seq.map (Seq.rev >> List.ofSeq)
    |> Seq.rev
    |> List.ofSeq

[<Fact>]
let ``splits graph`` () =
    let input =
        [ 1
          4
          5
          6
          7
          10
          11
          12
          15
          16
          19 ]

    splitGraph input
    |> Seq.toList
    |> should
        equal
           [ [ 1 ]
             [ 4; 5; 6; 7 ]
             [ 10; 11; 12 ]
             [ 15; 16 ]
             [ 19 ] ]

[<Fact>]
let ``splits another graph`` () =
    let input = [ 1; 3; 5; 6; 9; 11; 12 ]

    splitGraph input
    |> Seq.toList
    |> should equal [ [ 1; 3; 5; 6 ]; [ 9; 11; 12 ] ]



let rec countPaths (input: int list) =
    let head :: tail = input
    match tail with
    | [] -> 1L
    | [ a ] ->
        let pathA =
            if a - head <= 3 then Some(countPaths (input |> List.skip 1)) else None

        [ pathA ] |> List.choose id |> List.reduce (+)
    | [ a; b ] ->
        let pathA =
            if a - head <= 3 then Some(countPaths (input |> List.skip 1)) else None

        let pathB =
            if b - head <= 3 then Some(countPaths (input |> List.skip 2)) else None

        [ pathA; pathB ]
        |> List.choose id
        |> List.reduce (+)
    | a :: b :: c :: _ ->
        let pathA =
            if a - head <= 3 then Some(countPaths (input |> List.skip 1)) else None

        let pathB =
            if b - head <= 3 then Some(countPaths (input |> List.skip 2)) else None

        let pathC =
            if c - head <= 3 then Some(countPaths (input |> List.skip 3)) else None

        [ pathA; pathB; pathC ]
        |> List.choose id
        |> List.reduce (+)

[<Fact>]
let ``countPaths counts paths`` () =
    countPaths [ 4; 5; 6; 7 ] |> should equal 4L

[<Fact>]
let ``countPaths counts more paths`` () =
    countPaths [ 4; 5; 6 ] |> should equal 2L

[<Fact>]
let ``countPaths counts even more paths`` () =
    countPaths [ 1; 2; 3; 4; 5 ] |> should equal 7L

let solve10b (input: int seq) =
    let splits =
        input
        |> Seq.append [ 0 ]
        |> Seq.sort
        |> splitGraph

    splits |> Seq.map countPaths |> Seq.reduce (*)

[<Fact>]
let ``solves 10b example`` () =
    solve10b Input.exampleInput |> should equal 8L

[<Fact>]
let ``solves 10b second example`` () =
    solve10b Input.example2 |> should equal 19208L

[<Fact>]
let ``solves 10b problem`` () =
    solve10b Input.problemInput
    |> should equal 12089663946752L

(*
new idea:

when you look at a whiteboard diagram, you can see that there are
never more than 3 edges running concurrently. so maybe think of this
problem as pulling on multiple ropes, where no rope is permitted to
get longer than n (n=3 for the problem)

*)

type Rope =
    { startVal: int
      endVal: int
      numPathsRepresented: int64 }
(*
start with a single rope, e.g.:

0 -> 2 (representing 1 path)

when you add #3, turn that into two ropes:
2 -> 3 (representing 1 path)
0 -> 3 (representing 1 path)

add #4:

2 -> 4 (represents 1 path)
3 -> 4 (represents 2 paths. (the two ropes that previously terminated in "3"))

*)

let rec go (ropes: Rope list) (nums: int list) =
    if nums |> List.isEmpty then
        ropes
    else
        let head :: tail = nums

        let newRopes =
            List.collect (fun r ->
                // we can always add a new segment, from the current rope end to the next start
                let rope1 =
                    Some
                        { r with
                              startVal = r.endVal
                              endVal = head }

                // we might be able to also add one from the old segment start
                let rope2 =
                    if (head - r.startVal <= 3) then Some { r with endVal = head } else None

                let ropes = [ rope1; rope2 ] |> List.choose id
                ropes) ropes
            
        let mergeRopes a b = {a with numPathsRepresented = a.numPathsRepresented + b.numPathsRepresented}
        let d = newRopes |> List.groupBy id |> List.map (fun (_,ropeList) -> ropeList |> List.reduce mergeRopes)

        go d tail

let ropeSolve (input: int seq) =
    let sorted = input |> Seq.toList |> List.sort
    let head :: tail = sorted

    let first: Rope =
        { startVal = 0
          endVal = head
          numPathsRepresented = 1L }

    let result = go [ first ] tail
    result |> List.map (fun r -> r.numPathsRepresented) |> List.reduce (+)

[<Fact>]
let ``solves 10b example the new way`` () =
    ropeSolve Input.exampleInput |>should equal 8L
    
[<Fact>]
let ``solves 10b example 2 the new way`` () =
    ropeSolve Input.example2 |>should equal 19208L
    
[<Fact>]
let ``solves 10b problem the new way`` () =
    ropeSolve Input.problemInput |>should equal 12089663946752L