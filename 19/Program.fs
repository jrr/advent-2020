
open Solve

[<EntryPoint>]
let main argv =
    
    let one = replaceNumWithString Input.problemInput 7 "a"
    let two = replaceNumWithString one 13 "b"
    let lines = two |> Common.lineGroups |> Seq.head
    let rightSide = lines |> Seq.map (fun s -> s.Split(":").[1])
    rightSide |> Seq.iter (printfn "%s")
//    printfn "%s" two

    0
