#load "./Helper.fsx"
#load "./Range.fsx"
open Helper
open Range

// Day 4: Camp Cleanup - Range unions and intersections
//
// Part 1 - Check for range subsets
// Part 2 - Check for range unions

let part1 sectionAssignments =
    sectionAssignments
    |> Seq.filter (fun (section1, section2) ->
        match Range.tryIntersect section1 section2 with
        | Some intersection -> intersection = section1 || intersection = section2
        | None -> false)
    |> Seq.length
    // Correct Answer: 526, took: 361µs

let part2 sectionAssignments =
    sectionAssignments
    |> Seq.choose ((<||) Range.tryUnion)
    |> Seq.length
    // Correct Answer: 886, took: 352µs

let sectionAssignments =
    Puzzle.readLinesWithHashComments "day04.txt"
    |> Seq.map (String.capture "(\d+)-(\d+),(\d+)-(\d+)")
    |> Seq.map (fun [x1; x2; y1; y2] ->
        { From = int64 x1; To = int64 x2 }, { From = int64 y1; To = int64 y2})

[for _ in 1..3 do part1 sectionAssignments |> ignore ] // warmup the script runner for accurate timings

Puzzle.measurePart1µs part1 sectionAssignments
Puzzle.measurePart2µs part2 sectionAssignments
