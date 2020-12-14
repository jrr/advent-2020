
open Solve

[<EntryPoint>]
let main argv =
    
    let result = Input.problemInput |> parse |> solveTwo
//    let result = Input.exampleInput|> parse |> solveTwo
    printfn "Found answer! %u" (result)
    0 // return an integer exit code
