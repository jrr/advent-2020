module Solve

open System

type Parsed =
    { range: int * int
      char: char
      string: string }

let parse (input: string) =
    let what = input.Split ' ' |> List.ofArray
    let a :: b :: c :: tail = what
    if tail.Length > 0 then failwith "parse error"
    let a1 :: a2 :: tail = a.Split '-' |> List.ofArray
    if tail.Length > 0 then failwith "parse error"

    { range = (int a1, int a2)
      char = b.[0]
      string = c }

let inRange (range: int * int) (input: int) = fst range <= input && input <= snd range

let validate (record: Parsed) =
    let counts = record.string |> Seq.groupBy id

    let howManyOfTheSpecifiedChar =
        counts
        |> Seq.tryFind (fun (x, y) -> x = record.char)
        |> function
        | None -> 0
        | Some x -> x |> snd |> Seq.length

    inRange record.range howManyOfTheSpecifiedChar



let innerSolve (input: string) (validate: Parsed -> bool) =
    input.Split '\n'
    |> Seq.map (fun l -> l.Trim())
    |> Seq.filter (fun l -> l.Length <> 0)
    |> Seq.map parse
    |> Seq.filter validate
    |> Seq.length
    
let solve (input: string) =
    innerSolve input validate

let validate2 (record:Parsed) =
    let char1 = record.string.[fst record.range - 1]
    let char2 = record.string.[snd record.range - 1]
    (char1 = record.char) <> (char2 = record.char)
let solve2 (input:string) =
    innerSolve input validate2