
open Solve

[<EntryPoint>]
let main argv =
    
    Input.exampleInput |> parse |> print |> printfn "%s"

    0
