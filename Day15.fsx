#load "./Helper.fsx"
open Helper

// Day 15: Beacon Exclusion Zone
//
// Approach:
//   Part (1) - Scanned every point in the row and checked the manhattan
//              distance to all the scanners to see if it's contained.
//   Part (2) - Any non-scanned space must be just outside a scanner's range.
//              The code will go through each scanner, and check each position
//              just beyond it's range to see if it's within another scanners
//              range. If it's not that's our missing beacon location!


type Sensor = { Pos : int * int; Range : int }

let manhattanDistance ((ax, ay), (bx, by)) = (abs (ax - bx) + abs (ay - by))

let sensorContains pos sensor =
     manhattanDistance (sensor.Pos, pos) <= sensor.Range

let sensorSurrounds sensor =
     let posX, posY = sensor.Pos
     let range = sensor.Range + 1
     let topHalf =
          [ for i in -range..range do (i, range - abs i) ]
          |> List.map (fun (x,y) -> (posX + x, posY + y))
     let bottomHalf = topHalf |> List.map (fun (x, y) -> x, -y)
     topHalf@bottomHalf |> List.distinct

let part1 sensorsAndBeacons =
     let sensors =
          sensorsAndBeacons
          |> List.map (fun (sensorPos, beaconPos) ->
               { Pos = sensorPos; Range = manhattanDistance (sensorPos, beaconPos) })

     let beacons = sensorsAndBeacons |> List.unzip |> snd |> Set

     let leftMost = sensors |> List.map (fun s -> (fst s.Pos) - s.Range) |> List.min
     let rightMost = sensors |> List.map (fun s -> (fst s.Pos) + s.Range) |> List.max

     let testY = 2000000 // 10
     [ for i in leftMost..rightMost do
            if sensors |> List.exists (sensorContains (i, testY))
            then (i, testY) ]
     |> List.filter (fun x -> Set.contains x beacons |> not)
     |> List.length
     // Correct Answer: 5040643, took: 338,076µs

let part2 sensorsAndBeacons =
     let sensors =
          sensorsAndBeacons
          |> List.map (fun (sensorPos, beaconPos) ->
               { Pos = sensorPos; Range = manhattanDistance (sensorPos, beaconPos) })

     let maxValue = 4000000 // 20

     let x, y =
          sensors
          |> List.pick (fun s ->
               sensorSurrounds s
               |> List.filter (fun (x,y) -> x >= 0 && x <= maxValue &&
                                            y >= 0 && y <= maxValue)
               |> List.tryFind (fun (x,y) ->
                    sensors |> List.forall (sensorContains (x,y) >> not)))
     (int64 x * 4000000L) + int64 y
     // Correct Answer: 11016575214126, took: 548,631µs

let sensorsAndBeacons =
    Puzzle.readLinesWithHashComments "day15.txt"
    |> Seq.map (String.capture "=(-?\d+).*=(-?\d+).*=(-?\d+).*=(-?\d+)" >>
                fun [sx;sy;bx;by] -> (int sx, int sy), (int bx, int by))
    |> Seq.toList

Puzzle.measurePart1µs part1 sensorsAndBeacons
Puzzle.measurePart2µs part2 sensorsAndBeacons