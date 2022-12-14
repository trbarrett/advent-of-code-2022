#load "./Helper.fsx"
open Helper

// Day 1 : Calorie Counting - Counting totals of groups
//
// Part 1 - Sum each group and find the max
// Part 2 - Sum each group and find the total of the largest 3

let part1 numbers =
    numbers
    |> List.map List.sum
    |> List.max
    // Correct Answer: 74394, took: 32µs

let part2 numbers =
    numbers
    |> List.map List.sum
    |> List.sortDescending
    |> List.take 3
    |> List.sum
    // Correct Answer: 212836, took: 230µs

let numbers =
    Puzzle.readLinesWithHashComments "day01.txt"
    |> splitOnEmptyLines
    |> List.map (List.map int32)

Puzzle.measurePart1µs part1 numbers
Puzzle.measurePart1µs part2 numbers
