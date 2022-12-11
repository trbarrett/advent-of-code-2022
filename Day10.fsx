#load "./Helper.fsx"
open Helper

// Day 10: Cathode-Ray Tube
//
// Approach: Rather that trying to keep track of the cycle time of various
//           instructions I replaced each `addx X` instr with a `noop` & the
//           original `addx X` instruction. Then every instruction was exactly
//           one cylce, and the rest of it was quite straightforward.

type Instruction = | Noop | AddX of int

let expandInstructions instrs =
    instrs |> List.collect (function
        | Noop -> [Noop]
        | AddX x -> [Noop; AddX x])

let calculateCycleValues instrs =
    ([1], instrs)
    ||> List.fold (fun acc instr ->
        let x::_ = acc
        match instr with
        | Noop -> x::acc
        | AddX addX -> x + addX::acc)
    |> List.rev

let part1 instrs =
    let cycleValues = instrs |> expandInstructions |> calculateCycleValues

    [20; 60; 100; 140; 180; 220]
    |> List.map (fun x -> List.item (x-1) cycleValues |> (*) x)
    |> List.sum
    // Correct Answer: 12740, took: 5µs

let rayPosition x = x % 40
let xInRayPosition x ray = Set [x-1;x;x+1] |> Set.contains (rayPosition ray)

let part2 instrs =
    let cycleValues = instrs |> expandInstructions |> calculateCycleValues

    cycleValues |> List.iteri (fun i x ->
        if rayPosition i = 0 then printfn ""
        if xInRayPosition x i then printf "#" else printf ".")
    // Correct Answer: RBPARAGF, took: 105µs

let parseInstr = function
    | "noop" -> Noop
    | Capture "addx (-?\d+)" [addx] -> AddX (int addx)
    | str -> failwithf "Unexpected input: %s" str

let instrs =
    Puzzle.readLinesWithHashComments "day10.txt"
    |> Seq.map parseInstr
    |> Seq.toList

[for _ in 1..3 do part1 instrs |> ignore ] // warmup the script runner for accurate timings

Puzzle.measurePart1µs part1 instrs
Puzzle.measurePart2µs part2 instrs