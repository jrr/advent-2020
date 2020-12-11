module Tests

open System
open Xunit
open FsUnit.Xunit
open Solve

type ``Helpers`` () =
    

        
    [<Fact>]
    let ``parses board`` () =
        let result = parse Input.exampleInput
        result.[1].[1] |> should equal OpenSeat
        
    [<Fact>]
    let ``parses small board`` () =
        let result = parse """.L#
    #L.
    """
        result |> should equal [|[|Floor;OpenSeat;OccupiedSeat|];[|OccupiedSeat;OpenSeat;Floor|] |]
        
    [<Fact>]
    let ``unparses small board`` () =
        
        [|[|Floor;OpenSeat;OccupiedSeat|];[|OccupiedSeat;OpenSeat;Floor|] |] |> unparse
            |> should equal """
.L#
#L.
"""

        
    [<Fact>]
    let ``recognizes equivalent boards`` () =
        let b1 = [|[|Floor;OpenSeat;OccupiedSeat|];[|OccupiedSeat;OpenSeat;Floor|] |]
        let b2 = [|[|Floor;OpenSeat;OccupiedSeat|];[|OccupiedSeat;OpenSeat;Floor|] |]
        boardEq b1 b2 |> should equal true
        
    [<Fact>]
    let ``detects mismatched boards`` () =
        let b1 = [|[|Floor;OpenSeat;OccupiedSeat|];[|OccupiedSeat;Floor;Floor|] |]
        let b2 = [|[|Floor;OpenSeat;OccupiedSeat|];[|OccupiedSeat;OpenSeat;Floor|] |]
        boardEq b1 b2 |> should equal false
        
type ``11A`` () =
    [<Fact>]
    let ``example round 1`` () =
        Input.exampleInput |> parse |> tick11a |> unparse |> should equal """
#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##
"""

    [<Fact>]
    let ``example round 2`` () =
        """
#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##
"""
        |> parse |> tick11a |> unparse |> should equal """
#.LL.L#.##
#LLLLLL.L#
L.L.L..L..
#LLL.LL.L#
#.LL.LL.LL
#.LLLL#.##
..L.L.....
#LLLLLLLL#
#.LLLLLL.L
#.#LLLL.##
"""

        
    [<Fact>]
    let ``solves 11a example`` () =
        Input.exampleInput |> parse |> solve11a |> should equal {NumRounds=5;OccupiedSeats=37}
        
    [<Fact>]
    let ``solves 11a problem`` () =
        Input.problemInput |> parse |> solve11a |> should equal {NumRounds=115;OccupiedSeats=2126}
type ``11B`` () =
    
    [<Fact>]
    let ``example round 2`` () =
        """
#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##
"""
        |> parse |> tick11b |> unparse |> should equal """
#.LL.LL.L#
#LLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLL#
#.LLLLLL.L
#.LLLLL.L#
"""

    [<Fact>]
    let ``raycast example 1`` () =
        let input = parse """
.............
.L.L.#.#.#.#.
............."""
        raycast input (0,1) (1,1) |> should equal [Neighbor Floor; Neighbor OpenSeat]
        
    [<Fact>]
    let ``solves 11b example`` () =
        Input.exampleInput |> parse |> solve11b |> should equal {NumRounds=6;OccupiedSeats=26}
        
    [<Fact>]
    let ``solves 11b problem`` () =
        Input.problemInput |> parse |> solve11b |> should equal {NumRounds=84;OccupiedSeats=1914}
