module Tests

open System
open Xunit
open FsUnit.Xunit
open Solve

[<Fact>]
let ``solves problem`` () =
    solve "foo" |> should equal "foo"
    
let input1 =
    """
1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
    """
    
[<Fact>]
let ``parses line`` () =
    parse "1-3 a: abcde" |> should equal {range=(1,3);char='a';string="abcde"}

    
[<Fact>]
let ``validates good`` () =
    validate {range=(1,3);char='a';string="abcde"} |> should equal true
[<Fact>]
let ``validates bad`` () =
    validate {range=(1,3);char='z';string="abcde"} |> should equal false
