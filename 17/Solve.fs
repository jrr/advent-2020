module Solve

type Point2 = int * int
type Point3 = int * int * int

type PointOps<'T when 'T: comparison> =
    abstract print: 'T -> string
    abstract neighborVectors: 'T seq
    abstract addPoint: 'T -> 'T -> 'T

let neighborVectors2: Point2 seq =
    [ -1; 0; 1 ]
    |> Seq.map (fun x -> [ -1; 0; 1 ] |> Seq.map (fun y -> x, y))
    |> Seq.concat

let neighborVectors3: Point3 seq =
    [ -1; 0; 1 ]
    |> Seq.map (fun x ->
        [ -1; 0; 1 ]
        |> Seq.map (fun y -> [ -1; 0; 1 ] |> Seq.map (fun z -> x, y, z)))
    |> Seq.concat
    |> Seq.concat

let oneOf2 (x, _) = x
let twoOf2 (_, y) = y
let oneOf3 (x, _, _) = x
let twoOf3 (_, y, _) = y
let threeOf3 (_, _, z) = z

let addPoint2 (p1: Point2) (p2: Point2): Point2 =
    (oneOf2 p1 + oneOf2 p2), (twoOf2 p1 + twoOf2 p2)

let addPoint3 (p1: Point3) (p2: Point3): Point3 =
    (oneOf3 p1 + oneOf3 p2), (twoOf3 p1 + twoOf3 p2), (threeOf3 p1 + threeOf3 p2)

type Point2Ops() =
    interface PointOps<Point2> with
        member this.print(x) = "jkl"
        member this.neighborVectors = neighborVectors2
        member this.addPoint a b = addPoint2 a b


type Point3Ops() =
    interface PointOps<Point3> with
        member this.print(x) = "jkl"
        member this.neighborVectors = neighborVectors3
        member this.addPoint a b = addPoint3 a b


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


let neighbors (pointOps: PointOps<'t>) (p: 't) =
    pointOps.neighborVectors
    |> Seq.map (fun n -> pointOps.addPoint n p)
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


let point3ops = Point3Ops()
let point2ops = Point2Ops()

let tick (pointOps: PointOps<'P>) (input: Set<'P>) =
    let pointsToTest =
        input
        |> Set.map (neighbors pointOps)
        |> Set.unionMany

    let newPoints =
        pointsToTest
        |> Set.filter (fun p ->
            let isActive = input.Contains p

            let numNeighbors =
                p
                |> (neighbors pointOps)
                |> Set.filter (fun n -> input.Contains n)
                |> Set.count

            match isActive, numNeighbors with
            | true, 2 -> true
            | true, 3 -> true
            | false, 3 -> true
            | _ -> false

            )

    newPoints

let tick3 = tick point3ops
let tick2 = tick point2ops

let point3toPoint2 (p: Point3): Point2 = (oneOf3 p), (twoOf3 p)
let point3toPoint3 (p: Point3): Point3 = p

let solve2d (input: string) =
    let ops = point2ops
    input
    |> parse
    |> Set.map (point3toPoint2)
    |> tick ops
    |> tick ops
    |> tick ops
    |> tick ops
    |> tick ops
    |> tick ops
    |> fun s -> s.Count

let solveOne (input: string) =
    let ops = point3ops
    input
    |> parse
    |> tick ops
    |> tick ops
    |> tick ops
    |> tick ops
    |> tick ops
    |> tick ops
    |> fun s -> s.Count

let rec times n f input =
    let result = f input
    if n > 1 then times (n - 1) f result else result

let solveTwo (input: string) = input
