module Test

open Common
open System
open Xunit
open FsUnit.Xunit

[<Fact>]
let ``parseLines parses lines`` () =
    """
asdf
jkl
qwer"""
    |> nonEmptyLines
    |> List.ofSeq
    |> should equal [ "asdf"; "jkl"; "qwer" ]

[<Fact>]
let ``parseLineGroups parses groups`` () =
    """
asdf
jkl

qwer
uio

zxcv"""
    |> lineGroups
    |> Seq.map (List.ofSeq)
    |> List.ofSeq
    |> should
        equal
           [ [ "asdf"; "jkl" ]
             [ "qwer"; "uio" ]
             [ "zxcv" ] ]
