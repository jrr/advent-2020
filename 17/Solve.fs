module Solve

type Point2 = int * int
type Point3 = int * int * int
type Point4 = int * int * int * int

let tuple1to2 y x = (x, y)
let tuple2to3 z (x, y) = (x, y, z)
let tuple3to4 w (x, y, z) = (x, y, z, w)

let neighborVectors2: Point2 seq =
    [ -1; 0; 1 ]
    |> Seq.map (fun x -> [ -1; 0; 1 ] |> Seq.map (tuple1to2 x))
    |> Seq.concat

let neighborVectors3: Point3 seq =
    [ -1; 0; 1 ]
    |> Seq.map (fun z -> neighborVectors2 |> Seq.map (tuple2to3 z))
    |> Seq.concat

let neighborVectors4: Point4 seq =
    [ -1; 0; 1 ]
    |> Seq.map (fun w -> neighborVectors3 |> Seq.map (tuple3to4 w))
    |> Seq.concat

let oneOf2 (x, _) = x
let twoOf2 (_, y) = y
let oneOf3 (x, _, _) = x
let twoOf3 (_, y, _) = y
let threeOf3 (_, _, z) = z
let oneOf4 (i, _, _, _) = i
let twoOf4 (_, i, _, _) = i
let threeOf4 (_, _, i, _) = i
let fourOf4 (_, _, _, i) = i

let addPoint2 (p1: Point2) (p2: Point2): Point2 =
    (oneOf2 p1 + oneOf2 p2), (twoOf2 p1 + twoOf2 p2)

let addPoint3 (p1: Point3) (p2: Point3): Point3 =
    (oneOf3 p1 + oneOf3 p2), (twoOf3 p1 + twoOf3 p2), (threeOf3 p1 + threeOf3 p2)

let addPoint4 (p1: Point4) (p2: Point4): Point4 =
    (oneOf4 p1 + oneOf4 p2), (twoOf4 p1 + twoOf4 p2), (threeOf4 p1 + threeOf4 p2), (fourOf4 p1 + fourOf4 p2)



let parseLine (input: string) (y: int): Point3 seq =
    input
    |> Seq.mapi (fun x c -> if (c = '#') then Some(x, y, 0) else None)
    |> Seq.choose id

let parse (input: string) =
    input
    |> Common.nonEmptyLines
    |> Seq.mapi (fun i line -> parseLine line i)
    |> Seq.concat
    |> Set.ofSeq

let neighborsOf neighborVectors addPoint p =
    neighborVectors
    |> Seq.map (fun n -> addPoint n p)
    |> Seq.filter (fun r -> r <> p)
    |> Set.ofSeq

let minMax ps lookup =
    let values = ps |> Set.map lookup
    (values |> Seq.min), (values |> Seq.max)

let printPoint ps p =
    if ps |> Set.contains p then "#" else "."

let print ps =
    let minX, maxX = minMax ps oneOf3
    let minY, maxY = minMax ps twoOf3
    let minZ, maxZ = minMax ps threeOf3

    { minZ .. maxZ }
    |> Seq.map (fun z ->
        let slice =
            { minY .. maxY }
            |> Seq.map (fun y ->
                { minX .. maxX }
                |> Seq.map (fun x -> printPoint ps (x, y, z))
                |> String.concat "")
            |> String.concat "\n"

        sprintf "z=%d\n%s\n" z slice)
    |> String.concat "\n"

let tick neighborsFn (input: Set<'P>) =
    let pointsToTest =
        input |> Set.map neighborsFn |> Set.unionMany

    let newPoints =
        pointsToTest
        |> Set.filter (fun p ->
            let isActive = input.Contains p

            let numNeighbors =
                p
                |> neighborsFn
                |> Set.filter (fun n -> input.Contains n)
                |> Set.count

            match isActive, numNeighbors with
            | true, 2 -> true
            | true, 3 -> true
            | false, 3 -> true
            | _ -> false

            )

    newPoints

let tick3 =
    tick (neighborsOf neighborVectors3 addPoint3)

let tick2 =
    tick (neighborsOf neighborVectors2 addPoint2)

let point3toPoint2 (p: Point3): Point2 = (oneOf3 p), (twoOf3 p)
let point3toPoint3 (p: Point3): Point3 = p
let point3toPoint4 (p: Point3): Point4 = tuple3to4 0 p

let countAfterSixTicks neighbors input =
    input
    |> tick neighbors
    |> tick neighbors
    |> tick neighbors
    |> tick neighbors
    |> tick neighbors
    |> tick neighbors
    |> fun s -> s.Count

let solve2d (input: string) =
    let neighborsFn = neighborsOf neighborVectors2 addPoint2
    input
    |> parse
    |> Set.map (point3toPoint2)
    |> countAfterSixTicks neighborsFn

let solvePartOne (input: string) =
    let neighborsFn = neighborsOf neighborVectors3 addPoint3
    input |> parse |> countAfterSixTicks neighborsFn

let solvePartTwo (input: string) =
    let neighborsFn = neighborsOf neighborVectors4 addPoint4
    input
    |> parse
    |> Set.map (point3toPoint4)
    |> countAfterSixTicks neighborsFn
