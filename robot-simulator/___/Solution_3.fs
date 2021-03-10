module RobotSimulator

type Bearing =
    | North
    | South
    | East
    | West

type Turn =
    | Left
    | Right

let createRobot bearing pos = (bearing, pos)

let rightTurns =
    [| North; East; South; West; North |]
    |> Array.pairwise
    |> Map

let leftTurns =
    rightTurns
    |> Seq.map (fun (KeyValue (a, b)) -> (b, a))
    |> Map

let turn direction (bearing, pos) =
    let turns =
        if direction = Left then
            leftTurns
        else
            rightTurns

    (turns.[bearing], pos)

let turnLeft = turn Left
let turnRight = turn Right

let offset =
    function
    | North -> (0, 1)
    | South -> (0, -1)
    | East -> (1, 0)
    | West -> (-1, 0)

let advance (bearing, (x, y)) =
    let (x', y') = offset bearing
    (bearing, (x + x', y + y'))

let simulate robot instructions =
    instructions
    |> Seq.map
        (function
        | 'A' -> advance
        | 'L' -> turnLeft
        | 'R' -> turnRight)
    |> Seq.fold (|>) robot
