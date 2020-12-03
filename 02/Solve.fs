module Solve
open System

let solve (input:string) =
    input

type Parsed = {
    range: int*int
    char: char
    string: string
}
let parse (input:string) =
    let what = input.Split ' ' |> List.ofArray
    let a::b::c::tail = what
    if tail.Length > 0 then
        failwith "parse error"
    let [a1;a2] = a.Split '-' |> List.ofArray
    
    {range=(int a1,int a2);char=b.[0];string=c}
    
let inRange (range: int*int) (input:int) =
    fst range <= input && input <= snd range
    
let validate (record:Parsed) =
    let counts = record.string |> Seq.groupBy id
    let howManyOfTheSpecifiedChar = counts |> Seq.tryFind (fun (x,y) -> x = record.char)
                                    |> function
                                    | None -> 0
                                    | Some x -> x |> snd |> Seq.length
    inRange record.range howManyOfTheSpecifiedChar
                                    