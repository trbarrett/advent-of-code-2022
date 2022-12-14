#load "./Helper.fsx"
open System.Collections.Generic
open Helper
open System

// Day 14: Regolith Reservoir
//
// Approach: For part 1 I used a F# set. That was proving to be too slow for
//           part 2, so for that I used a standard .Net HashSet instead. I
//           should probably have used a simple array for the best performance.

let expandRockLine ((x1, y1), (x2, y2)) =
    let xStep = Math.Clamp(x2 - x1, -1, 1)
    let yStep = Math.Clamp(y2 - y1, -1, 1)
    (x1, y1) |> List.unfold (fun (x, y) ->
        if (x - xStep, y - yStep) <> (x2, y2)
        then Some((x, y), (x + xStep, y + yStep))
        else None)

let expandRocks (rocks : (int * int) list) =
    rocks |> List.pairwise |> List.collect expandRockLine |> Set

let rec dropSandIntoAbyss (x,y) (state : Set<int * int>) maxY =
    if y > maxY
    then state // we've fallen in the abyss
    else
        if not (Set.contains (x, y+1) state)
        then dropSandIntoAbyss (x, y+1) state maxY
        elif not (Set.contains (x-1, y+1) state)
        then dropSandIntoAbyss (x-1, y+1) state maxY
        elif not (Set.contains (x+1, y+1) state)
        then dropSandIntoAbyss (x+1, y+1) state maxY
        else Set.add (x,y) state

let rec continuouslyDropSandIntoAbyss start prevState maxY =
    let state = dropSandIntoAbyss start prevState maxY
    if state = prevState
    then state
    else continuouslyDropSandIntoAbyss start state maxY

let rec dropSandOntoFloor (x,y) (state : HashSet<int * int>) floor =
    if y = floor - 1
    then state.Add(x,y) |> ignore; state // we've hit the floor
    elif not (state.Contains (x, y+1))
    then dropSandOntoFloor (x, y+1) state floor
    elif not (state.Contains (x-1, y+1))
    then dropSandOntoFloor (x-1, y+1) state floor
    elif not (state.Contains (x+1, y+1))
    then dropSandOntoFloor (x+1, y+1) state floor
    else state.Add(x,y) |> ignore; state

let rec continuouslyDropSandUntilBlocked start (state : HashSet<int * int>) maxY =
    let state = dropSandOntoFloor start state maxY
    if state.Contains(start)
    then state
    else continuouslyDropSandUntilBlocked start state maxY

let part1 rocks =
     let roof =
        (Set.empty, rocks)
        ||> List.fold (fun acc rocks -> Set.union acc (expandRocks rocks))

     let _, _, _, maxY = Set.getExtents roof
     let state = continuouslyDropSandIntoAbyss (500,0) roof maxY
     state.Count - roof.Count
     //Correct Answer: 1330, took: 36,666µs

let part2 rocks =
     let roof =
        (Set.empty, rocks)
        ||> List.fold (fun acc rocks -> Set.union acc (expandRocks rocks))

     let _, _, _, maxY = Set.getExtents roof
     let floor = maxY + 2
     let state = new HashSet<int * int>(roof)
     let state = continuouslyDropSandUntilBlocked (500,0) state floor
     state.Count - roof.Count
     // Correct Answer: 26139, took: 59,932µs

let rocks =
    Puzzle.readLinesWithHashComments "day14.txt"
    |> Seq.map (String.captures "(\d+),(\d+)" >>
                List.map (fun [x;y] -> int x, int y ))
    |> Seq.toList

[for _ in 1..3 do part1 rocks |> ignore ] // warmup the script runner for accurate timings

Puzzle.measurePart1µs part1 rocks
Puzzle.measurePart2µs part2 rocks