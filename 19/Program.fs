
open Solve

[<EntryPoint>]
let main argv =
    
//    let one = replaceNumWithString Input.problemInput 7 "a"
//    let two = replaceNumWithString one 13 "b"
//    let lines = two |> Common.lineGroups |> Seq.head
//    let rightSide = lines |> Seq.map (fun s -> s.Split(":").[1])
//    rightSide |> Seq.iter (printfn "%s")
//    printfn "%s" two
    let logLen msg a =
        printfn "%s length=%d" msg (a|>Seq.length)
        a
    let input = Input.problemInput |> parse
    let reduced = input.rules
                    |> reduceRepeatedly
//                    |> logLen "initial" 
//                    |> reduceStringRules 
//                    |>logLen "reduced strings"
//                    |>shortenStrings
//                    |>logLen "shortened strings" 
//                    |> reduceStringRules 
//                    |>logLen "reduced strings again"
//                    |>shortenStrings
//                    |>logLen "shortened strings again" 
    printRules reduced
    
    let dups = reduced |> Seq.groupBy snd |> Seq.filter (fun x -> snd x |> Seq.length > 1)
    printfn "%d dupes:" (dups |> Seq.length)

    0
