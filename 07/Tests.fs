module Tests

open System
open Xunit
open FsUnit.Xunit
open Solve


[<Fact>]
let ``parseLine parses`` () =
    parseLine "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
    |> should
        equal
           ("muted yellow",
            Some [ (2, "shiny gold")
                   (9, "faded blue") ])

    let x =
        parseLine "faded blue bags contain no other bags."

    Assert.StrictEqual(x, ("faded blue", None))


    "bright white bags contain 1 shiny gold bag."
    |> parseLine
    |> should equal ("bright white", Some [ (1, "shiny gold") ])

[<Fact>]
let flattens () =
    let result =
        ("muted yellow",
         Some [ (2, "shiny gold")
                (9, "faded blue") ])
        |> flattenLine

    Assert.StrictEqual
        (result,
         [ ("muted yellow", "shiny gold")
           ("muted yellow", "faded blue") ])


[<Fact>]
let ``builds map`` () =
    let shortInput = """
bright white bags contain 1 shiny gold bag.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
    """

    let result = buildMapOfEdges Input.exampleInput

    let expected =
        (

        [ ("bright white", [ "light red"; "dark orange" ])
          ("dark olive", [ "shiny gold" ])
          ("dotted black", [ "dark olive"; "vibrant plum" ])
          ("faded blue",
           [ "muted yellow"
             "dark olive"
             "vibrant plum" ])
          ("muted yellow", [ "light red"; "dark orange" ])
          ("shiny gold", [ "bright white"; "muted yellow" ])
          ("vibrant plum", [ "shiny gold" ]) ]

        |> Map.ofSeq)

    result |> should equal expected
//    Assert.StrictEqual(result, expected)

//     ()

[<Fact>]
let ``solves 7a example`` () =
    let map = buildMapOfEdges Input.exampleInput

    let result =
        findOtherColorsContaining map "shiny gold"

    result |> Seq.length |> should equal 4
    result
    |> should
        equal
           [ "bright white"
             "light red"
             "dark orange"
             "muted yellow" ]


[<Fact>]
let ``solves 7a problem`` () =
    let map = buildMapOfEdges Input.problemInput

    let result =
        findOtherColorsContaining map "shiny gold"

    result |> Seq.length |> should equal 355
