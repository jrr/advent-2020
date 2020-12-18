module Solve
open System
open Common

    
type Token =
    | IntLiteral of int64
    | Plus
    | MultipliedBy
//    | LeftParen
//    | RightParen
    
let regexParse (s:char list) =
    let str = s |> String.Concat
    match str with
    | Regex @"(\d+)(.*)" [ num;tail ] -> num,tail
    | x -> failwith (sprintf "parse error on '%s'" x)
    
let rec tokenizeRec (s:char list) =
    match s with
    | '+'::tail -> Plus::(tokenizeRec tail)
    | '*'::tail -> MultipliedBy::(tokenizeRec tail)
//    | '(' -> Some LeftParen
//    | ')' -> Some RightParen
    | ' '::tail -> tokenizeRec tail
    | [] -> []
    | other ->
        let n,tail = regexParse other
        (IntLiteral (int64 n))::(tail |> Seq.toList |> tokenizeRec)
let tokenize (s:string) =
    s |> List.ofSeq |> tokenizeRec
    
type ParenSplit = {left:string;parenthesized:string;right:string}
let extractParenthesized (input:string) =
    match input with
    | Regex @"^(.*)\(([^\)]+)\)(.*)$" [left;parenthesized;right] -> {left=left;parenthesized=parenthesized;right=right}
    | _ -> failwith "failed to parse parens"
//        | "hcl", Regex @"#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]" [] -> true
    
    
type ParenCount =
    | HasParens of int
    | NoParens
    
let countParens s =
    let left = s |> Seq.filter (fun c -> c = '(') |> Seq.length
    let right = s |> Seq.filter (fun c -> c = ')') |> Seq.length
    match left,right with
    | 0,0 -> NoParens
    | l,r when l = r -> HasParens l
    | l,r when l <> r -> failwith "Mismatched parens"
    
let rec evalPart1 (input:Token list) =
    match input with
    | [IntLiteral n] -> n
    | (IntLiteral n)::Plus::tail -> n + (evalPart1 tail)
    | (IntLiteral n)::MultipliedBy::tail -> n * (evalPart1 tail)
    | x -> failwith (sprintf "unrecognized token %O" x)
    
let rec evalPart2 (input:Token list) =
    match input with
    | (IntLiteral n)::Plus::(IntLiteral m)::tail ->
        evalPart2 ((IntLiteral (n+m))::tail)
    | (IntLiteral n)::MultipliedBy::tail -> n * (evalPart2 tail)
    | [IntLiteral n] -> n
    | x -> failwith (sprintf "unrecognized token %O" x)
    
let evalString eval (input:string) =
    let tokens = input |> tokenize |> Seq.toList |> List.rev
    eval tokens |> (sprintf "%d")
    
let rec recSolve (eval:Token list->int64) (input:string) : string =
    match countParens input with
    | NoParens -> evalString eval input
    | HasParens _ ->
        let splits = extractParenthesized input
        let solvedMiddle = evalString  eval splits.parenthesized
        let newString = sprintf "%s%s%s" splits.left solvedMiddle splits.right
        recSolve eval newString
        
let solveLine (input:string) =
    recSolve evalPart1 input |> int64
    
let solveLine2 (input:string) =
    recSolve evalPart2 input |> int64
    

let solveOne (input: string) =
    input |> Common.nonEmptyLines |> Seq.map solveLine  |> Seq.reduce (+)

let solveTwo (input: string) =
    input |> Common.nonEmptyLines |> Seq.map solveLine2  |> Seq.reduce (+)
