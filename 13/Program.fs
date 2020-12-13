
open Solve

[<EntryPoint>]
let main argv =
    
    let result = Input.exampleInput |> parse |> solveTwo
    printfn "Found answer! %d" (result)
    0 // return an integer exit code
