module Tests

open System
open Xunit
open FsUnit.Xunit
open Solve


type ``Helpers`` () =
    
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
        
    [<Fact>]
    let ``vanEck`` () =
        ecK () |> Seq.take 5 |> List.ofSeq |> should equal [0; 0; 1; 0; 2]

type ``Part One`` () =
    [<Fact>]
    let ``small example next digits`` () =
        let s = buildSeq [0;3;6]
        s |> Seq.take 2 |> List.ofSeq |> should equal [0;3]
        s |> Seq.take 7 |> List.ofSeq |> should equal [0;3;3;1;0;4;0]
        
    [<Fact>]
    let ``solves small example`` () =
        [0;3;6] |> solveOne |> should equal 436
       
    [<Fact>]
    let ``more examples`` () =
        [1;3;2] |> solveOne |> should equal 1
        [2;1;3] |> solveOne |> should equal 10
        [1;2;3] |> solveOne |> should equal 27
        [2;3;1] |> solveOne |> should equal 78
        [3;2;1] |> solveOne |> should equal 438
        [3;1;2] |> solveOne |> should equal 1836
        
    [<Fact>]
    let ``solves problem`` () =
        [14;8;16;0;1;17] |> solveOne |> should equal 240
        
type ``Part Two`` () =
    [<Fact(Skip="slow")>]
    let ``solves example`` () =
        [0;3;6] |> solveTwo |> should equal 175594
        
    [<Fact(Skip="slow")>]
    let ``solves problem`` () =
        [14;8;16;0;1;17] |> solveTwo |> should equal 0
