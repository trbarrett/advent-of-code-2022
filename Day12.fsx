#load "./Helper.fsx"
#load "./AStar.fsx"
open Helper
open AStar

module AOA = ArrayOfArrays

// Day 12: Hill Climbing Algorithm
//
// Approach: Use the astar algorithm we created last year.
//           For part 2 we use a DU so the starting pos is connected to all the
//           lowest height positions at a weight of 0.

type Pos = | AnyLowest | Pos of (int * int)

let adjacent (row, col) =
    [ (row - 1, col); (row + 1, col); (row, col - 1); (row, col + 1)]

let surrounding hill pos =
    adjacent pos
    |> List.choose (fun pos -> ArrayOfArrays.tryGeti pos hill)

let neighbours hill pos =
    match pos with
    | Pos pos ->
        let currHeight = ArrayOfArrays.get pos hill
        surrounding hill pos
        |> List.filter (fun (pos, h) -> h <= currHeight + 1)
        |> List.unzip |> fst
        |> List.map (fun pos -> Pos pos, 1.) // weight of each connection is 1
    | AnyLowest ->
        hill
        |> ArrayOfArrays.findIndexes ((=) 0)
        // connected to all lowest with 0 weight
        |> Array.map (fun pos -> Pos pos, 0.)
        |> Array.toList

let heuristic pos (Pos (goalRow, goalCol)) =
    match pos with
    | Pos (row, col) ->
        // simple manhattan distance for the heuristic, since no diagonal movement
        (abs (row - goalRow) + abs (col - goalCol)) |> double
    | AnyLowest -> System.Double.PositiveInfinity // worst possible since we don't know

let part1 (hill, startPos, endPos) =
     let (Some (_, cost)) =
        astar (Pos startPos) (Pos endPos) (neighbours hill) heuristic
     cost //Correct Answer: 350, took: 1,609µs

let part2 (hill, _, endPos) =
     let (Some (_, cost)) =
        astar AnyLowest (Pos endPos) (neighbours hill) heuristic
     cost // Correct Answer: 349, took: 1,339µs

let alphaToInt ch =
    match ch with
    | 'S' -> 0 | 'E' -> 25
    | _ -> int ch - int 'a'

let hillData =
    let charGrid =
        Puzzle.readLinesWithHashComments "day12.txt"
        |> Seq.map (Seq.toArray)
        |> Seq.toArray

    let (Some startPos) = ArrayOfArrays.tryFindIndex ((=) 'S') charGrid
    let (Some endPos) = ArrayOfArrays.tryFindIndex ((=) 'E') charGrid
    let hill = ArrayOfArrays.map alphaToInt charGrid
    hill, startPos, endPos

[for _ in 1..3 do part1 hillData |> ignore ] // warmup the script runner for accurate timings

Puzzle.measurePart1µs part1 hillData
Puzzle.measurePart2µs part2 hillData