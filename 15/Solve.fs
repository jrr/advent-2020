module Solve

let rec recSeq (input: int list) =
    seq {
        let head :: tail = input

        let next =
            match tail |> Seq.tryFindIndex (fun x -> x = head) with
            | Some previousUse -> previousUse + 1
            | None -> 0

        printfn "next %d" next
        yield next
        yield! recSeq (next :: input)
    }

let buildSeq (input: int list) = recSeq (input |> List.rev)

// https://rosettacode.org/wiki/Van_Eck_sequence#F.23
// Generate Van Eck's Sequence. Nigel Galloway: June 19th., 2019
let ecK () =
    let n =
        System.Collections.Generic.Dictionary<int, int>()

    Seq.unfold (fun (g, e) ->
        Some
            (g,
             ((if n.ContainsKey g then
                 let i = n.[g]
                 n.[g] <- e
                 e - i
               else
                   n.[g] <- e
                   0),
              e + 1))) (0, 0)



let recSolve (input: int list) (num: int) =
    input
    |> buildSeq
    |> Seq.skip (num - input.Length - 1)
    |> Seq.take 1
    |> Seq.head


let solveOne (input: int list) = recSolve input 2020

let solveTwo (input: int list) = recSolve input 30000000
