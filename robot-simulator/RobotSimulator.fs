module RobotSimulator

type Direction =
    | North
    | East
    | South
    | West

type Position = int * int

type Robot =
    { direction: Direction
      position: Position }

let create direction position =
    { direction = direction
      position = position }

let turnRightDirection direction =
    match direction with
    | North -> East
    | East -> South
    | South -> West
    | West -> North

let turnRight robot = 
    create (turnRightDirection robot.direction) robot.position

let turnLeftDirection direction =
    match direction with
    | North -> West
    | East -> North
    | South -> East
    | West -> South

let turnLeft robot = 
    create (turnLeftDirection robot.direction) robot.position
    

let advance robot =
    let (a, b) = robot.position
    match robot.direction with
    | North -> create robot.direction (a, b+1)
    | West -> create robot.direction (a-1, b)
    | South -> create robot.direction (a, b-1)
    | East -> create robot.direction (a+1, b)

let apply instructions robot =
    match instructions with
    | 'R' -> turnRight robot
    | 'L' -> turnLeft robot
    | 'A' -> advance robot
    | _ -> robot

let rec moveList (instructions:List<char>) robot =
    match instructions with
    | head::tail -> moveList tail ( apply head robot )
    | [] -> robot

let move (instructions:string) =
    moveList (Seq.toList instructions)
