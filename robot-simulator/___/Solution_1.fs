module RobotSimulator

type Bearing =
    | North
    | East
    | South
    | West

type Robot =
    { bearing: Bearing
      position: int * int }

let createRobot bearing position =
    { bearing = bearing
      position = position }

let turnLeft robot =
    let newBearing =
        match robot.bearing with
        | North -> West
        | East -> North
        | South -> East
        | West -> South

    { robot with bearing = newBearing }

let turnRight = turnLeft >> turnLeft >> turnLeft

let advance ({ bearing = bearing; position = (x, y) } as robot) =
    let newPosition =
        match bearing with
        | North -> (x, y + 1)
        | East -> (x + 1, y)
        | South -> (x, y - 1)
        | West -> (x - 1, y)

    { robot with position = newPosition }

let simulate robot instructions =
    let toAction =
        function
        | 'L' -> turnLeft
        | 'R' -> turnRight
        | 'A' -> advance
        | _ -> id

    instructions
    |> Seq.map toAction
    |> Seq.fold (|>) robot
