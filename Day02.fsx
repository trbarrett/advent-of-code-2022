#load "./Helper.fsx"
open Helper

// Day 2: Rock Paper Scissors - Winning and losing at rock papers scissors
//
// Part 1 - Calculate the round and game scores
// Part 2 - Determine the hand shape to get the expected outcome, then get the scores

type HandShape = Rock | Paper | Scissors
type Outcome = Win | Lose | Draw

module HandShape =
    let fromChar = function
        | 'A' | 'X' -> Rock
        | 'B' | 'Y' -> Paper
        | 'C' | 'Z' -> Scissors

    let score = function
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

module Outcome =
    let fromChar = function
        | 'X' -> Lose
        | 'Y' -> Draw
        | 'Z' -> Win

    let score = function
        | Win -> 6
        | Lose -> 0
        | Draw -> 3

let roundScore (opponent, us) =
    let outcome =
        match opponent, us with
        | Rock, Paper -> Win     | Rock, Scissors -> Lose
        | Paper, Scissors -> Win | Paper, Rock -> Lose
        | Scissors, Rock -> Win  | Scissors, Paper -> Lose
        | _ -> Draw

    Outcome.score outcome + HandShape.score us

let handShapeToAchieveOutcome (opponent, outcome) =
    match opponent, outcome with
    | x, Draw -> x
    | Rock, Win -> Paper     | Rock, Lose -> Scissors
    | Paper, Win -> Scissors | Paper, Lose -> Rock
    | Scissors, Win -> Rock  | Scissors, Lose -> Paper

let part1 strategyGuide =
    strategyGuide
    |> Seq.map (fun (x,y) -> HandShape.fromChar x, HandShape.fromChar y)
    |> Seq.sumBy roundScore
    // Correct Answer: 13484, took: 2ms

let part2 strategyGuide =
    strategyGuide
    |> Seq.map (fun (x,y) -> HandShape.fromChar x, Outcome.fromChar y)
    |> Seq.map (fun (x,o) -> x, handShapeToAchieveOutcome (x,o))
    |> Seq.sumBy roundScore
    // Correct Answer: 13433, took: 2ms

let strategyGuide =
    Puzzle.readLinesWithHashComments "day02.txt"
    |> Seq.map (fun str -> str.[0], str.[2])

Puzzle.measurePart1 part1 strategyGuide
Puzzle.measurePart2 part2 strategyGuide
