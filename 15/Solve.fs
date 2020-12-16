module Solve

type MutableDictionary = System.Collections.Generic.Dictionary<int, int>

let newDict (s: Map<int, int>) =
    System.Collections.Generic.Dictionary<int, int>(s)

let tryGet (dict: MutableDictionary) key =
    if dict.ContainsKey key then Some dict.[key] else None

let rec vanEckInner (dict: MutableDictionary) (turn: int) (current: int) =
    seq {
        yield current

        let newValue =
            match tryGet dict current with
            | Some prev -> turn - prev
            | None -> 0

        dict.[current] <- turn
        yield! vanEckInner dict (turn + 1) newValue
    }

let vanEckSeq (input: int list) =
    seq {
        for item in input do
            yield item

        let seed =
            (input
             |> Seq.mapi (fun i num -> num, i + 1)
             |> Map.ofSeq)

        let dict = newDict seed

        yield! vanEckInner dict (input.Length + 1) 0 (* is that right? *)
    }

let getNth (input: int list) (num: int) =
    vanEckSeq input
    |> Seq.skip (num - 1)
    |> Seq.take 1
    |> Seq.head

let solve15a (input: int list) = getNth input 2020

let solve15b (input: int list) = getNth input 30000000
