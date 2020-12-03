module Solve

let solve (input:string) =
    input

let parse (input:string) =
    input.Split '\n' |> Seq.ofArray |> Seq.map (fun s -> s.Trim()) |> Seq.filter (fun s -> s <> "")
    
let lookup (input: string seq) (loc: int*int) =
    let (x,y) = loc
    let row = input |> Seq.tryItem y
    row |> Option.map (fun r -> r |> Seq.item (x % r.Length) )

let addTup a b =
    (fst a + fst b),(snd a + snd b)
    
let rec drive (input:string seq) (pos:int*int) (slope:int*int) =
    let r = lookup input pos
    let newPos = addTup pos slope
    match r with
    | None -> 0
    | Some '#' ->
        1 + drive input newPos slope
    | Some '.' ->
        0 + drive input newPos slope
