
open Solve

[<EntryPoint>]
let main argv =
    
    let result = Input.problemInput |> parse |> solveTwo
    result |> Seq.iter (fun (n,i) ->
        printfn "%s : %d" n i
        )
//    printfn "Found answer! %u" (result)
    0 // return an integer exit code
