module Tests

open System
open Xunit
open FsUnit.Xunit
open Solve

[<Fact>]
let ``solves problem`` () = solve "foo" |> should equal "foo"



let input = """
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
"""

let parseLine (s:string) =
    let sides = s.Split(" bags contain ")
    let outerColor = sides.[0]
    
    let innerColors =
        if sides.[1] = "no other bags." then
            None
        else
            sides.[1].Replace(".","").Split(',')
            |> Seq.map (fun c ->
                let words = c.Replace(" bags","").Trim().Split(' ') |> List.ofArray
                let number::color = words
                (int number,(color|>String.concat " "))
            )
            |> Seq.toList
            |> Some
        
    (outerColor,innerColors)


[<Fact>]
let ``parseLine parses``() =
    parseLine "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags." |> should equal ("muted yellow",Some [(2,"shiny gold");(9,"faded blue")])
    
    let x = parseLine "faded blue bags contain no other bags."
    fst x |> should equal "faded blue"
    snd x |> should equal None
    
[<Fact>]
let stuff () =
    input
    |> Common.lineSeq
    |> Seq.map parseLine
    |> Seq.length
    |> should greaterThan 3
