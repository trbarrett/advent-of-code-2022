#load "./Helper.fsx"
open Helper

// Day 8: Treetop Tree House
//
// Part 1 - We only need to check each row & column once from each direction
//          and remember the largest one we've seen to know for each tree if
//          it's visible from that direction
//
// Part 2 - Brute force of checking each cardinal direction for each tree. There
//          are much faster ways, but given it only takes 11ms it's not worth
//          optimising

let lookEast (rowNo, colNo) (trees : int [][]) =
    [| for c in colNo..(Array.length trees.[rowNo] - 1) do
           (rowNo, c), trees.[rowNo].[c] |]

let lookWest (rowNo, colNo) (trees : int [][]) =
    [| for c in colNo..(-1)..0 do
           (rowNo, c), trees.[rowNo].[c] |]

let lookSouth (rowNo, colNo) (trees : int [][]) =
    [| for r in rowNo..(Array.length trees - 1) do
           (r, colNo), trees.[r].[colNo] |]

let lookNorth (rowNo, colNo) (trees : int [][]) =
    [| for r in rowNo..(-1)..0 do
           (r, colNo), trees.[r].[colNo] |]

let getVisible (trees : ((int * int) * int) []) =
    ((Set.empty, -1), trees)
    ||> Array.fold (fun (acc, prevLargest) (pos, size) ->
        if size > prevLargest
        then (Set.add pos acc, size)
        else acc, prevLargest)
    |> fst

let getTreeCover (rowNo, colNo) (trees : int [][]) =
    let treeHeight = trees.[rowNo].[colNo]

    [| lookNorth; lookEast; lookSouth; lookWest |]
    |> Array.map (fun f ->
        f (rowNo, colNo) trees
        |> Array.skip 1
        |> Array.takeWithFirst (fun (_,t) -> t < treeHeight)
        |> Array.length)
    |> Array.reduce (*)

let getBestTreeCover (trees : int [][]) =
    [ for rowNo in 0..(Array.length trees - 1) do
        for colNo in 0..(Array.length trees.[0] - 1) do
            yield (rowNo, colNo) ]
    |> List.map (fun pos -> getTreeCover pos trees)
    |> List.max

let allRowsAndColumns (trees : int [][]) =
    let height = Array.length trees
    let width = Array.length trees.[0]
    Array.concat
        [| [| for r in 0..(height - 1) do lookEast (r, 0) trees |]
           [| for r in 0..(height - 1) do lookWest (r, width - 1) trees |]
           [| for c in 0..(width - 1) do lookSouth (0, c) trees |]
           [| for c in 0..(width - 1) do lookNorth (height - 1, c) trees |] |]

let part1 trees =
    allRowsAndColumns trees
    |> Array.map getVisible
    |> Set.unionMany
    |> Set.count
    // Correct Answer: 1717, took: 821µs

let part2 trees =
   getBestTreeCover trees
    // Correct Answer: 549173, took: 11,318µs

let trees = Puzzle.readLinesWithHashComments "day08.txt"
            |> Seq.map (Seq.map Char.digitToInt >> Seq.toArray)
            |> Seq.toArray

[for _ in 1..3 do part1 trees |> ignore ] // warmup the script runner for accurate timings

Puzzle.measurePart1µs part1 trees
Puzzle.measurePart2µs part2 trees