module Solve
open System
open System.Collections.Generic

    
let rec findPairIn (sum:int) (input: LinkedList<int>) =
    let first = input.First.Value
    let last = input.Last.Value
    
    if first+last= sum then
        first,last
    else
        if first+last < sum then
            input.RemoveFirst()
            findPairIn sum input
        else
            input.RemoveLast()
            findPairIn sum input
    
let findPair (sum: int) (input: int list) =
    let sorted = input |>List.sort
    let ll = new LinkedList<int>(sorted) // cheating
    findPairIn sum ll
    
let solve (input:int list) =
    let (a,b) = findPair 2020 input
    a * b
