#load "./Helper.fsx"
open Helper

// Day 7: No Space Left On Device
//
// Approach: We keep track of the pwd (present working directory) via a list of
//           strings as we fold through the terminal lines. Note the path will
//           be in reverse order: e.g ["e";"a";"/"]
//
//           We use a map with an key for every full dir path and the value as
//           the total size on that path. Every time we encounter a file listing
//           we update the map with the file's size for every parent,
//           grandparent, etc's path.
//           e.g.
//           [ ["e";"a";"/"], 100
//             ["a";"/"],     200
//             ["/"],         300 ]

type TerminalLine =
    | Cd of string
    | FileListing of int64 * string

let parseLine (str : string) =
    match str with
    | "$ ls" -> None // we don't need the "ls" or "dir" lines
    | StartsWith "dir" _ -> None
    | Capture "\$ cd (.*)" [cdDir] ->
        Some (Cd cdDir)
    | Capture "(\d+) (.*)" [fileSize; fileName] ->
        Some (FileListing (int64 fileSize, fileName))
    | _ -> failwithf $"Could not parse string: %s{str}"

let rec addSizeToEachSubPathInPwd pwd fileSize dirMap =
    match pwd with
    | [] -> dirMap
    | pwd ->
        let dirMap = dirMap |> Map.replace pwd 0L ((+) fileSize)
        addSizeToEachSubPathInPwd (List.tail pwd) fileSize dirMap

let totalSizes (lines : TerminalLine list) =
    ((["/"], Map.empty), lines)
    ||> List.fold (fun (pwd, dirMap) line ->
        match line with
        | Cd arg ->
            match arg with
            | "/" -> ["/"], dirMap
            | ".." -> (List.tail pwd), dirMap
            | dirName -> (dirName::pwd), dirMap
        | FileListing (fileSize, _) ->
            pwd, addSizeToEachSubPathInPwd pwd fileSize dirMap)
   |> snd

let part1 lines =
    let totalSizes = totalSizes lines
    totalSizes
    |> Map.toList
    |> List.sumBy (fun (_,size) -> if size <= 100000 then size else 0)
    // Correct Answer: 1367870, took: 85µs

let part2 lines =
    let totalSizes = totalSizes lines

    let usedDiskSpace = totalSizes |> Map.find ["/"]
    let remainingDiskSpace = 700_000_00L - usedDiskSpace
    let needDiskSpace = 30_000_000L - remainingDiskSpace

    totalSizes
    |> Map.values |> List.ofSeq
    |> List.sort
    |> List.find (fun size -> size >= needDiskSpace)
    // Correct Answer: 549173, took: 514µs

let lines = Puzzle.readLinesWithHashComments "day07.txt"
            |> Seq.choose parseLine
            |> Seq.toList

[for _ in 1..3 do part1 lines |> ignore ] // warmup the script runner for accurate timings

Puzzle.measurePart1µs part1 lines
Puzzle.measurePart2µs part2 lines