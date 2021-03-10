module RobotSimulator

type Bearing =
    | North
    | East
    | South
    | West

type Point = int * int
type Robot = { Bearing: Bearing; Location: Point }

let createRobot b p = { Bearing = b; Location = p }

let left =
    function
    | North -> West
    | East -> North
    | South -> East
    | West -> South

let right =
    function
    | North -> East
    | East -> South
    | South -> West
    | West -> North

let movement =
    function
    | North -> (0, 1)
    | East -> (1, 0)
    | South -> (0, -1)
    | West -> (-1, 0)

let (+) ((x1, y1): Point) ((x2, y2): Point) = (x1 + x2, y1 + y2)

let turnLeft robot =
    { robot with
          Bearing = left robot.Bearing }

let turnRight robot =
    { robot with
          Bearing = right robot.Bearing }

let advance robot =
    { robot with
          Location = robot.Location + (robot.Bearing |> movement) }

let simulate robot commands =
    Seq.fold
        (fun r c ->
            match c with
            | 'R' -> turnRight r
            | 'L' -> turnLeft r
            | 'A' -> advance r
            | _ -> r)
        robot
        commands
