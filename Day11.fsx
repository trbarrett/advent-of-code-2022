#load "./Helper.fsx"
open Helper

// Day 11: Monkey in the Middle

type Operator = | Multiply | Sum
type Operand = | Old | Const of int64

type Monkey =
    { MonkeyNo : int
      Operator : Operator
      Operand : Operand
      ModTest : int64
      TrueMonkey : int
      FalseMonkey : int }

let parseMonkey (strs : string list) =
    let strs = strs |> List.toArray
    let [monkeyNo] = String.capture "Monkey (\d+):" strs.[0]
    let startingItems =
        String.captures "(\d+)" strs.[1]
        |> List.map List.head |> List.map int64
    let [operator; operand] =
        String.capture "Operation: new = old ([*+]) (\w+)" strs.[2]
    let [modTest] = String.capture "Test: divisible by (\d+)" strs.[3]
    let [trueMonkey] = String.capture "If true: throw to monkey (\d+)" strs.[4]
    let [falseMonkey] = String.capture "If false: throw to monkey (\d+)" strs.[5]

    let operator =
        match operator with
        | "*" -> Multiply
        | "+" -> Sum
        | _ -> failwithf "Unexpected operator: %s" operator

    let operand =
        match operand with
        | "old" -> Old
        | str -> Const (int64 str)

    { MonkeyNo = int monkeyNo
      Operator = operator
      Operand = operand
      ModTest = int64 modTest
      TrueMonkey = int trueMonkey
      FalseMonkey = int falseMonkey }
    , startingItems

let monkeyDo reduceWorry monkey item =
    let operand =
        match monkey.Operand with
        | Old -> item
        | Const x -> x

    let newValue =
        match monkey.Operator with
        | Multiply -> item * operand
        | Sum -> item + operand

    let newValue = reduceWorry newValue
    if newValue % monkey.ModTest = 0L
    then monkey.TrueMonkey, newValue
    else monkey.FalseMonkey, newValue

let performMonkey monkeyDo monkey (items, inspectCount) =
    let monkeyItems = Map.find monkey.MonkeyNo items
    let monkeyResults =
        monkeyItems
        |> List.map (monkeyDo monkey)
        |> List.groupByTuple |> Map
    let items = items |> Map.add monkey.MonkeyNo [] // clear this monkey's items
    let items = Map.merge (fun a b -> a@b) items monkeyResults
    let inspectCount =
        inspectCount
        |> Map.update monkey.MonkeyNo 0L (fun x ->
            x + int64 (List.length monkeyItems))
    items, inspectCount

let rec gcd x y =
    if y = 0L then x
    else gcd y (x % y)

let lcm a b = a*b/(gcd a b)

let performRound monkeyDo monkeys (items, inspectCount) =
    ((items, inspectCount), monkeys)
    ||> List.fold (flip (performMonkey monkeyDo))

let calculateMonkeyBusiness inspectCount =
    inspectCount
    |> Map.values
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.reduce (*)

let part1 (monkeys, startingItems) =
    let reduceWorry x = x / 3L
    let performRound = performRound (monkeyDo reduceWorry) monkeys
    let calc = Seq.replicate 20 performRound |> Seq.reduce (>>)
    let _, inspectCount = calc (startingItems, Map.empty)
    calculateMonkeyBusiness inspectCount
    // Correct Answer: 61005, took: 83µs

let part2 (monkeys, startingItems) =
    let lcm =
        monkeys
        |> List.map (fun m -> m.ModTest)
        |> List.reduce lcm

    let reduceWorry x = x % lcm
    let performRound = performRound (monkeyDo reduceWorry) monkeys
    let calc = Seq.replicate 10000 performRound |> Seq.reduce (>>)
    let _, inspectCount = calc (startingItems, Map.empty)
    calculateMonkeyBusiness inspectCount
    // Correct Answer: 20567144694, took: 67,496µs

let monkeysAndItems =
    let monkeys, items =
        Puzzle.readLinesWithHashComments "day11.txt"
        |> splitOnEmptyLines
        |> Seq.map parseMonkey
        |> Seq.toList
        |> List.unzip

    monkeys, items |> List.indexed |> Map

[for _ in 1..3 do part1 monkeysAndItems |> ignore ] // warmup the script runner for accurate timings

Puzzle.measurePart1µs part1 monkeysAndItems
Puzzle.measurePart2µs part2 monkeysAndItems