#load "./Helper.fsx"
open Helper

// Day 13:Distress Signal
//
// Approach: Wrote a simple recursive parser. The comparisons were a little
//           tricky but nothing too bad.

type Item =
    | Value of int
    | Lst of Item list

// https://www.reddit.com/r/adventofcode/comments/zkqst1/2022_day_13_json_is_for_the_weak/
let tokeniser (str: string) =
    str |> String.captures "(\[|\]|\d+)" |> List.map List.head

let parsePaket (xs : string list) =
    let rec parsePaket' (xs : string list) acc =
        match xs, acc with
        | [], _ -> [], acc
        | "]"::xs, Lst acc ->
            let xs, inner = parsePaket' xs (Lst []) // build the next inner paket
            parsePaket' xs (Lst (inner::acc))
        | "["::xs, acc -> xs, acc // we've finished building our inner list
        | digits::xs, Lst acc ->
            let value = Value (int digits)
            parsePaket' xs (Lst (value::acc))
        | _, Value _ -> failwith "Value type unexpected"
    parsePaket' (List.rev xs) (Lst []) |> snd

type RightOrder = | Yes | No | Unknown

let rec inRightOrder left right =
    let rec listInRightOrder left right =
        match left, right with
        | [], [] -> Unknown
        | [], _ -> Yes
        | _, [] -> No
        | x::xs, y::ys ->
            match inRightOrder x y with
            | Unknown -> listInRightOrder xs ys
            | x -> x

    match left, right with
    | Value left, Value right ->
        if left < right then Yes
        elif left > right then No
        else Unknown
    | Lst left, Lst right ->
        listInRightOrder left right
    | Value left, Lst right ->
        inRightOrder (Lst [Value left]) (Lst right)
    | Lst left, Value right ->
        inRightOrder (Lst left) (Lst [Value right])

let comparePart1 [left; right] = inRightOrder left right

let comparePart2 left right =
    match inRightOrder left right with
    | Yes -> -1 | Unknown -> 0 | No -> 1

let part1 signal =
     signal
     |> List.map comparePart1
     |> List.indexed
     |> List.choose (fun (i, res) ->
         match res with
         | Yes -> Some (i+1)
         | _ -> None)
     |> List.sum
     //Correct Answer: 5675, took: 6µs

let part2 signal =
     let signal = List.collect id signal
     let dividerPaket2 = (Lst [Lst [Value 2]])
     let dividerPaket6 = (Lst [Lst [Value 6]])
     let signal = dividerPaket2::dividerPaket6::signal
     let sorted = List.sortWith comparePart2 signal
     ((List.findIndex ((=) dividerPaket2) sorted) + 1)
     * ((List.findIndex ((=) dividerPaket6) sorted) + 1)
     // Correct Answer: 20383, took: 107µs

let signal =
    Puzzle.readLinesWithHashComments "day13.txt"
    |> Seq.map tokeniser
    |> Seq.toList
    |> Seq.split []
    |> List.map (List.map (Seq.toList >> parsePaket))

[for _ in 1..3 do part1 signal |> ignore ] // warmup the script runner for accurate timings

Puzzle.measurePart1µs part1 signal
Puzzle.measurePart2µs part2 signal