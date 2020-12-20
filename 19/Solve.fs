module Solve

open System
open System.Text.RegularExpressions
open Common

type NumOrString =
    | Num of int
    | String of string

type Rule =
    | StringRule of string
    | Digits of NumOrString list
    | Or of (NumOrString list) * (NumOrString list)

type ProblemInput =
    { rules: (int * Rule) list
      messages: string list }

let printDigits (d: NumOrString list) =
    d
    |> Seq.map (fun dee ->
        match dee with
        | Num i -> sprintf "%d" i
        | String s -> s

        )
    |> Seq.reduce (fun a b -> $"{a} {b}")

let printRules (rules: (int * Rule) seq) =
    rules
    |> Seq.iter (fun (i, rule) ->
        let s =
            match rule with
            | StringRule sr -> sr
            | Digits d -> d |> printDigits
            | Or (a, b) -> $"{printDigits a}|{printDigits b}"

        //        printfn "%s" s
        printfn "%d: %s" i s)

    ()

let parseLineOfInts (s: string) =
    s.Split(" ")
    |> Seq.filter (fun x -> x <> "")
    |> Seq.map int
    |> Seq.map Num
    |> Seq.toList

let parseOrLine (s: string) =
    let splits = s.Split("|")
    (parseLineOfInts splits.[0]), (parseLineOfInts splits.[1])

let parseRule (input: string) =
    match input with
    | Regex @"^(\d+): ([\d ]+)$" [ i; s ] -> (int i, (Digits(parseLineOfInts s)))
    | Regex @"^(\d+): ([\d \|]+)$" [ i; s ] -> (int i, (Or(parseOrLine s)))
    | Regex """^(\d+): "([ab])"$""" [ i; s ] -> (int i, StringRule s)
    | z -> failwith (sprintf "unrecognized input %s" z)

let parse (input: string) =
    let groups = input |> Common.lineGroups |> Seq.toList

    let rules =
        groups.[0] |> Seq.map parseRule |> List.ofSeq

    let messages = groups.[1] |> Seq.toList
    { rules = rules; messages = messages }

let replaceNumWithString (input: string) (num: int) (replacement: string) =
    Regex.Replace(input, $"([^\d]){num}([^^\d:])", $"$1{replacement}$2")

let applyStringToDigitList (digitList: NumOrString list) ((i, s): (int * string)) =
    digitList
    |> List.map (fun d ->
        match d with
        | Num x when x = i -> String s
        | other -> other)

let applyStringRule (stringRule: int * string) (sr: (int * Rule) seq) =
    sr
    |> Seq.map (fun (i, rule) ->
        i,
        match rule with
        | StringRule _ -> failwith "unexpected StringRule"
        | Digits digitList -> Digits(applyStringToDigitList digitList stringRule)
        | Or (leftList, rightList) ->
            Or(applyStringToDigitList leftList stringRule, applyStringToDigitList rightList stringRule))

let reduceStringRules (input: (int * Rule) list) =
    let isStringRule =
        (fun (i, r) ->
            match r with
            | StringRule _ -> true
            | _ -> false)

    let stringRules =
        input
        |> Seq.choose (fun r ->
            match r with
            | i, StringRule s -> Some(i, s)
            | _ -> None)

    let nonStringRules =
        input |> Seq.filter (isStringRule >> not)

    let updated =
        Seq.fold (fun rules stringRules -> applyStringRule stringRules rules) nonStringRules stringRules
        |> List.ofSeq

    printfn
        "Applied %d string rules, reducing rules from %d -> %d"
        (stringRules |> Seq.length)
        (input.Length)
        (updated.Length)
    updated

let reduceDigits (d: NumOrString list) =
    List.fold (fun accum element ->
        match accum |> List.tryLast with
        | None -> List.append accum [ element ]
        | Some last ->
            match last, element with
            | String a, String b ->
                let withoutLast = accum.[..accum.Length - 2]
                List.append withoutLast [ (String($"{a}{b}")) ]
            | _ -> List.append accum [ element ]) [] d

let maybeConvertToStringRule (rule: Rule) =
    match rule with
    | Digits [ String s ] -> StringRule s
    | x -> x

let shortenStrings (input: (int * Rule) list) =
    input
    |> List.map (fun (i, rule) ->

        i,
        match rule with
        | StringRule sr -> StringRule sr
        | Digits d ->
            let squished = Digits(reduceDigits d)
            maybeConvertToStringRule squished
        | Or (a, b) -> Or(reduceDigits a, reduceDigits b)

        )

let rec reduceRepeatedly rules =
    let reduced =
        rules |> reduceStringRules |> shortenStrings

    if reduced.Length = rules.Length then
        printfn "done reducing at %d." (reduced.Length)
        reduced
    else
        printfn "reduced to %d.." (reduced.Length)
        reduceRepeatedly reduced


let solveOne (input: string) = input |> parse |> id

let solveTwo (input: string) = input |> parse |> id

type RuleMap = Map<int, Rule>

let buildRuleMap (rules: (int * Rule) list) = rules |> Map.ofList


type Node =
    | LeafNode of string
    | OrNode of Node list
    | SequenceNode of Node list

let rec buildTree (map: RuleMap) (from: int) =
    let rule = map.[from]
    match rule with
    | StringRule sr -> LeafNode sr
    | Digits d -> buildSequence map d
    | Or (a, b) ->
        OrNode [ (buildSequence map a)
                 (buildSequence map b) ]

and digitNode (map: RuleMap) (i: NumOrString) =
    match i with
    | Num num -> (buildTree map num)
    | String s -> LeafNode s

and buildSequence (map: RuleMap) (l: NumOrString list) =
    l
    |> List.map (fun d -> digitNode map d)
    |> SequenceNode


let rec walkMap (node: Node) (depth: int) =
    let indent =
        Seq.replicate depth "  " |> String.Concat
    // printfn "%swalking.. d=%d" indent depth

    let result =
        match node with
        | LeafNode s -> printfn "%sLeafNode! '%s'" indent s
        | OrNode nodeList ->
            printfn "%sOrNode!" indent
            nodeList
            |> List.map (fun n -> walkMap n (depth + 1))
            |> ignore
        | SequenceNode nodeList ->
            printfn "%sSequenceNode!" indent
            nodeList
            |> List.map (fun n -> walkMap n (depth + 1))
            |> ignore

    node



// 0: 4 1 5
// 1: 2 3 | 3 2
// 2: 4 4 | 5 5
// 3: 4 5 | 5 4
// 4: "a"
// 5: "b"
let n5 = LeafNode "b"
let n4 = LeafNode "a"

let n3 =
    OrNode [ (SequenceNode [ n4; n5 ])
             (SequenceNode [ n5; n4 ]) ]

let n2 =
    OrNode [ (SequenceNode [ n4; n4 ])
             (SequenceNode [ n5; n5 ]) ]

let n1 =
    OrNode [ (SequenceNode [ n2; n3 ])
             (SequenceNode [ n3; n2 ]) ]

let n0 = SequenceNode [ n4; n1; n5 ]
