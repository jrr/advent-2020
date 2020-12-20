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



/// calls given function on each node of the tree, and then traverse from the updated node
let rec walkMap (depth: int) fn (node: Node) =

    let updatedNode = fn node depth

    match updatedNode with
    | LeafNode s -> LeafNode s
    | OrNode nodeList ->
        nodeList
        |> List.map (fun n -> walkMap (depth + 1) fn n)
        |> OrNode
    | SequenceNode nodeList ->
        nodeList
        |> List.map (fun n -> walkMap (depth + 1) fn n)
        |> SequenceNode


let rec logNode (node: Node) (depth: int): Node =
    let indent =
        Seq.replicate depth "  " |> String.Concat

    match node with
    | LeafNode s ->
        printfn "%sLeafNode! '%s'" indent s
        LeafNode s
    | OrNode nodeList ->
        printfn "%sOrNode!" indent
        OrNode nodeList
    | SequenceNode nodeList ->
        printfn "%sSequenceNode!" indent
        SequenceNode nodeList


let printTree tree = walkMap 0 logNode tree |> ignore

let rec squishSequences (node: Node) (_depth: int): Node =
    match node with
    | SequenceNode ((LeafNode s1) :: (LeafNode s2) :: tail) -> SequenceNode((LeafNode $"{s1}{s2}") :: tail)
    | n -> n

let rec removeSequences (node: Node) (_depth: int): Node =
    match node with
    | SequenceNode [ (LeafNode s) ] -> LeafNode s
    | n -> n

let optimizeTree tree =
    tree
    |> walkMap 0 squishSequences
    |> walkMap 0 removeSequences


/// todo: separate traversal and logging
let rec walkMap2 (depth: int) fn (node: Node): string =

    // let updatedNode = fn node depth

    match node with
    | LeafNode s -> sprintf "%s" s
    | OrNode nodeList ->
        nodeList
        |> List.map (fun n -> walkMap2 (depth + 1) fn n)
        |> List.reduce (fun a b -> $"{a}|{b}")
        |> sprintf "(%s)"
    | SequenceNode nodeList ->
        nodeList
        |> List.map (fun n -> walkMap2 (depth + 1) fn n)
        |> List.reduce (fun a b -> $"{a},{b}")
        |> sprintf "[%s]"
