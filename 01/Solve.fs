module Solve

open System
open System.Collections.Generic


// find 3 in, which calls find 2 in, which calls find1 ..?
// ( recursive findN() )

let rec findPairIn (sum: int) (input: LinkedList<int>) =
    if input.Count < 2 then
        None
    else
        let first = input.First.Value
        let last = input.Last.Value
        if first + last = sum then
            Some(first, last)
        else if first + last < sum then
            input.RemoveFirst()
            findPairIn sum input
        else
            input.RemoveLast()
            findPairIn sum input

let findPair (sum: int) (input: int list) =
    let ll = new LinkedList<int>(input) // cheating
    findPairIn sum ll

let solve (input: int list) =
    let sorted = input |> List.sort
    let (a, b) = findPair 2020 sorted |> Option.get
    a * b

    
let rec findThree (sum: int) (input: int list) =
    
    let head::tail = input
    let r = findPair (sum-head) tail
    match r with
    | None -> findThree sum tail
    | Some (b,c) -> Some (head,b,c)
    
let solve3 (input:int list) =
    let sorted = input |> List.sort
    let (a,b,c) = findThree 2020 sorted |> Option.get
    a * b * c