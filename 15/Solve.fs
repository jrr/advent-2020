module Solve

let rec recSeq (input: int list) =
    seq {
        let head :: tail = input

        let next =
            match tail |> Seq.tryFindIndex (fun x -> x = head) with
            | Some previousUse -> previousUse + 1
            | None -> 0

        yield next
        yield! recSeq (next :: input)
    }

let buildSeq (input: int list) = recSeq (input |> List.rev)

let solveOne (input: int list) =
    input
    |> buildSeq
    |> Seq.skip (2020 - input.Length - 1)
    |> Seq.take 1
    |> Seq.head

let solveTwo (input: string) = input
