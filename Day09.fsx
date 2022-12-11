#load "./Helper.fsx"
open Helper

// Day 9: Rope Bridge
//
// Approach - It's a little tricky thinking through how the tail follows the
//            head, but once you've got that down and go through each step it's
//            fairly straightforwards

type Dir = | Right | Left | Up | Down

module Dir =
    let fromChar = function
        | 'R' -> Right
        | 'L' -> Left
        | 'U' -> Up
        | 'D' -> Down
        | ch -> failwithf "Invalid dir char %c" ch

let stepDist (hx, hy) (tx, ty) =
    max (abs (hx - tx)) (abs (hy - ty))

let moveInDir (x,y) dir =
    match dir with
    | Up ->    (x,   y+1) | Down -> (x,   y-1)
    | Right -> (x+1, y  ) | Left -> (x-1, y  )

let moveOneTowards a b =
    if a = b then a
    elif a > b then b + 1
    else b - 1

let segmentStep (hx, hy) (tx, ty) =
    if stepDist (hx, hy) (tx, ty) <= 1
    then (tx, ty) // DON'T MOVE if the tail is only one step away from the head
    else (moveOneTowards hx tx, moveOneTowards hy ty)

let moveRope prevRope dir =
    let newHead = moveInDir (List.head prevRope) dir
    // update each position in the rest of the rope
    ([newHead], List.tail prevRope)
    ||> List.fold (fun acc ropeSegment ->
        let head::_ = acc
        let newSegmentPos = segmentStep head ropeSegment
        newSegmentPos::acc)
    |> List.rev

let performMotion (rope, acc) (dir, stepCount) =
    // move 1 at a time through each of the given step counts
    ((rope, acc), [0..(stepCount-1)])
    ||> List.fold (fun (prevRope, acc) _ ->
        let rope = moveRope prevRope dir
        rope, Set.add (List.last rope) acc)

let part1 motions =
    let rope = [(0,0); (0,0)]
    let _, s =
        ((rope, Set [0,0]), motions)
        ||> List.fold performMotion
    s.Count
    // Correct Answer: 6067, took: 1,859µs

let part2 motions =
    let rope = List.replicate 10 (0,0)
    let _, s =
        ((rope, Set [0,0]), motions)
        ||> List.fold performMotion
    s.Count
    // Correct Answer: 2471, took: 2,175µs

let motions =
    Puzzle.readLinesWithHashComments "day09.txt"
    |> Seq.map (fun str ->
        let [dir; count] = String.capture "^(.) (\d+)" str
        Dir.fromChar dir.[0], int count)
    |> Seq.toList

[for _ in 1..3 do part1 motions |> ignore ] // warmup the script runner for accurate timings

Puzzle.measurePart1µs part1 motions
Puzzle.measurePart2µs part2 motions