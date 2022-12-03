#load "./Helper.fsx"
open Helper

// Day 3: Rucksack Reorganization - Set intersections
//
// Part 1 - Find the common item in the two compartments in each rucksack
// Part 2 - Find the common item within each group of three rucksacks

let letterPriority (c : char) =
    if c <= 'Z'
    then int c - int 'A' + 27
    else int c - int 'a' + 1

let findDuplicateItem (compartment1, compartment2) =
    Set.intersect (Set compartment1) (Set compartment2)
    |> Set.minElement

let part1 (rucksacks : seq<string>) =
    rucksacks
    |> Seq.map (fun str -> str.Substring(0, str.Length/2), str.Substring(str.Length/2) )
    |> Seq.map findDuplicateItem
    |> Seq.sumBy letterPriority
    // Correct Answer: 7746, took: 1ms

let part2 (rucksacks : seq<string>) =
    rucksacks
    |> Seq.map Set
    |> Seq.chunkBySize 3
    |> Seq.map (Set.intersectMany >> Set.minElement)
    |> Seq.sumBy letterPriority
    // Correct Answer: 2604, took: 1ms

let rucksacks = Puzzle.readLinesWithHashComments "day03.txt"

[for _ in 1..3 do part1 rucksacks |> ignore ] // warmup the script runner for accurate timings

Puzzle.measurePart1 part1 rucksacks
Puzzle.measurePart2 part2 rucksacks
