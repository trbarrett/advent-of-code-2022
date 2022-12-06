#load "./Helper.fsx"
open System.Numerics
open Helper

// Day 6: Tuning Trouble - Bit manipulation
//
// Approach: We can treat each character as a flag in a bit field by mapping it
//           to a number from 0-25 and performing 2^x (via a bit shift). From
//           there we just bitwise OR them together and we can count how many
//           flags are set vs what we need.
//
//           Using a set (below) is simpler, it's also 10 times slower
//           `(Set window) |> Set.count = Array.length window`

let charToFlag (ch : char) =
    1u <<< (int ch - int 'a')

let checkForMark (window : uint []) =
    BitOperations.PopCount (Array.reduce (|||) window) = (Array.length window)

let part1 (signal : string) =
    let windows = signal |> Seq.map charToFlag |> Seq.windowed 4
    (windows |> Seq.findIndex checkForMark) + 4
    // Correct Answer: 1965, took: 14µs

let part2 (signal : string) =
    let windows = signal |> Seq.map charToFlag |> Seq.windowed 14
    (windows |> Seq.findIndex checkForMark) + 14
    // Correct Answer: 2773, took: 35µs

let signal = Puzzle.readLinesWithHashComments "day06.txt" |> Seq.item 0

[for _ in 1..3 do part1 signal |> ignore ] // warmup the script runner for accurate timings

Puzzle.measurePart1µs part1 signal
Puzzle.measurePart2µs part2 signal
