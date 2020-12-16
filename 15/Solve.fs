module Solve


let tryGet (dict: System.Collections.Generic.Dictionary<int, int>) key =
    if dict.ContainsKey key then Some dict.[key] else None



let rec solve3inner (dict: System.Collections.Generic.Dictionary<int, int>) (turn: int) (current: int) =
    seq {
        let prevUse = tryGet dict current

        let newCurrent =
            match prevUse with
            | Some prev -> turn - prev
            | None -> 0

        dict.[current] <- turn
        yield current
        yield! solve3inner dict (turn + 1) newCurrent
    }

let vanEckSeq (input: int list) =

    seq {

        let seed =
            input
            |> Seq.mapi (fun i num -> num, i + 1)
            |> Map.ofSeq

        for item in input do
            yield item

        let dict =
            System.Collections.Generic.Dictionary<int, int>(seed)

        yield! solve3inner dict (input.Length + 1) 0 (* is that right? *)
    }

let getNth (input: int list) (num: int) =
    vanEckSeq input
    |> Seq.skip (num - 1)
    |> Seq.take 1
    |> Seq.head

let solve15a (input: int list) = getNth input 2020

let solve15b (input: int list) = getNth input 30000000
