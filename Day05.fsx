#load "./Helper.fsx"
open Helper

// Day 5: Supply Stacks - Complex parsing + moving data between lists of lists

type RearrangementProcedure = { MoveTimes : int; From : int; To : int }

let moveStack9000 moveTimes stackFrom stackTo =
    let moving, stackFrom = List.splitAt moveTimes stackFrom
    let stackTo = (List.rev moving) @ stackTo
    stackFrom, stackTo

let moveStack9001 moveTimes stackFrom stackTo =
    let moving, stackFrom = List.splitAt moveTimes stackFrom
    let stackTo = moving @ stackTo
    stackFrom, stackTo

let performRearrangements stacks rearrangements moveFn =
    (stacks, rearrangements)
    ||> Seq.fold (fun stacks rearrangement ->
        let fromStack = stacks |> List.item rearrangement.From
        let toStack = stacks |> List.item rearrangement.To
        let fromStack, toStack = moveFn rearrangement.MoveTimes fromStack toStack
        stacks |> List.replaceAt rearrangement.From fromStack
               |> List.replaceAt rearrangement.To toStack)

let part1 (stacks, rearrangements) =
    performRearrangements stacks rearrangements moveStack9000
    |> Seq.map List.head |> Seq.toString ""
    // Correct Answer: VQZNJMWTR, took: 146µs

let part2 (stacks, rearrangements) =
    performRearrangements stacks rearrangements moveStack9001
    |> Seq.map List.head |> Seq.toString ""
    // Correct Answer: NLCDCLVMQ, took: 139µs

let stacks, rearrangements =
    let [stacks; rearrangements] =
        Puzzle.readLinesWithHashComments "day05.txt"
        |> splitOnEmptyLines

    // we don't need the last line with just the stack numbers
    let stacks = stacks |> List.removeAt (List.length stacks - 1)

    let stacks =
        stacks
        |> Seq.map (String.captures "[[\s]([A-Z]|\s)[]\s]\s?" >> List.concat)
        // our list of lists is arranged in rows. But we want columns. So transpose it to fix that
        |> List.transpose
        |> List.map (List.choose (function | " " -> None | x -> Some x))

    let rearrangements =
        rearrangements
        |> Seq.map (String.capture "move (\d+) from (\d+) to (\d+)")
        |> Seq.map (fun [move; from; ``to``] ->
            // note we change "from" and "to" so that they're 0 indexed
            { MoveTimes = int move; From = (int from - 1); To = (int ``to`` - 1) })

    stacks, rearrangements

[for _ in 1..3 do part1 (stacks, rearrangements) |> ignore ] // warmup the script runner for accurate timings

Puzzle.measurePart1µs part1 (stacks, rearrangements)
Puzzle.measurePart2µs part2 (stacks, rearrangements)
