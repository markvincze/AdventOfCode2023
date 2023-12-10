open System
open System.IO

let lines = File.ReadAllLines "FSharp/10-pipe-maze-input.txt"

let maze = Array2D.init
               lines.[0].Length
               lines.Length
               (fun x y -> lines.[y].[x])

let allCoords maze =
    seq {
        for x in 0..(Array2D.length1 maze - 1) do
            for y in 0..(Array2D.length2 maze - 1) do
                yield (x, y)
    }

let startingPos (maze: char[,]) =
    allCoords maze
    |> Seq.find (fun (x, y) -> maze.[x, y] = 'S')

let findTwoConnections (maze: char[,]) (x, y) =
    let c = maze.[x, y]
    [
        (if x > 0 &&
           (c = 'S' || c = '-' || c = '7' || c = 'J') &&
           (match maze.[x - 1, y] with | '-' | 'L' | 'F' -> true | _ -> false)
        then Some (x - 1, y)
        else None)
        (if y > 0 &&
           (c = 'S' || c = '|' || c = 'L' || c = 'J') &&
           (match maze.[x, y - 1] with | '|' | '7' | 'F' -> true | _ -> false)
        then Some (x, y - 1)
        else None)
        (if x < (Array2D.length1 maze - 1) &&
           (c = 'S' || c = '-' || c = 'L' || c = 'F') &&
           (match maze.[x + 1, y] with | '-' | '7' | 'J' -> true | _ -> false)
        then Some (x + 1, y)
        else None)
        (if y < (Array2D.length2 maze - 1) &&
           (c = 'S' || c = '|' || c = 'F' || c = '7') &&
           (match maze.[x, y + 1] with | '|' | 'L' | 'J' -> true | _ -> false)
        then Some (x, y + 1)
        else None)
    ]
    |> List.choose id

let nextStep maze prev pos = findTwoConnections maze pos |> List.except [ prev ] |> List.exactlyOne

let findMaxDist maze =
    let rec findMaxDist maze pos1 prev1 pos2 prev2 dist =
        if pos1 = pos2
        then dist
        else findMaxDist
                 maze
                 (nextStep maze prev1 pos1)
                 pos1
                 (nextStep maze prev2 pos2)
                 pos2
                 (dist + 1)
    
    let [ pos1; pos2 ] = findTwoConnections maze (startingPos maze)

    let sp = startingPos maze
    findMaxDist maze pos1 sp pos2 sp 1

let result1 = findMaxDist maze

// Part 2
let rec getBorder maze last prev pos acc =
    if pos = last
    then last :: acc
    else getBorder maze last pos (nextStep maze prev pos) (pos :: acc)

let sp = startingPos maze
let [ c1; c2 ] = findTwoConnections maze sp
let border = getBorder maze c1 sp c2 []

type TurnDir = Left | Right

let turnDir (maze: char[,]) (prevX, prevY) (posX, posY) =
    match maze.[prevX, prevY], maze.[posX, posY] with
    | '-', 'J' | 'L', 'J' -> Some Left
    | 'F', 'J' when prevY = posY -> Some Left
    | '|', 'J' | '7', 'J' -> Some Right
    | 'F', 'J' when prevX = posX -> Some Right
    | '|', '7' | 'J', '7' -> Some Left
    | 'L', '7' when prevX = posX -> Some Left
    | '-', '7' | 'F', '7' -> Some Right
    | 'L', '7' when prevY = posY -> Some Right
    | '-', 'F' | '7', 'F' -> Some Left
    | 'J', 'F' when prevY = posY -> Some Left
    | '|', 'F' | 'L', 'F' -> Some Right
    | 'J', 'F' when prevX = posX -> Some Right
    | '|', 'L' | 'F', 'L' -> Some Left
    | '7', 'L' when prevX = posX -> Some Left
    | '-', 'L' | 'J', 'L' -> Some Right
    | '7', 'L' when prevY = posY -> Some Right
    | _ -> None

let turns = border
            |> List.pairwise 
            |> List.choose (fun (prev, pos) -> turnDir maze prev pos)

let lc = turns |> List.filter (fun t -> t = Left) |> List.length
let rc = turns |> List.filter (fun t -> t = Right) |> List.length

let borderClockwise = if lc > rc
                      then border |> List.rev
                      else border

let rightNeighbors (maze: char[,]) (prevX, prevY) (posX, posY) =
    match maze.[posX, posY] with
    | '-' -> if prevX = posX - 1 then Some [ (posX, posY + 1) ] else Some [ (posX, posY - 1) ]
    | '|' -> if prevY = posY - 1 then Some [ (posX - 1, posY) ] else Some [ (posX + 1, posY) ]
    | 'J' -> if prevX = posX - 1 then Some [ (posX, posY + 1); (posX + 1, posY) ] else None
    | 'F' -> if prevX = posX + 1 then Some [ (posX, posY - 1); (posX - 1, posY) ] else None
    | '7' -> if prevX = posX - 1 then None else Some [ (posX + 1, posY); (posX, posY - 1) ]
    | 'L' -> if prevX = posX + 1 then None else Some [ (posX - 1, posY); (posX, posY + 1) ]
    | _ -> None

let borderTiles = borderClockwise
                  |> List.pairwise
                  |> List.choose (fun (prev, pos) -> rightNeighbors maze prev pos)
                  |> List.collect id
                  |> List.distinct
                  |> List.filter (fun (x, y) -> List.contains (x, y) borderClockwise |> not)

let neighborTiles (maze: char[,]) (x, y) =
    [
        if x > 0 && List.contains (x - 1, y) borderClockwise |> not then yield (x - 1, y)
        if y > 0 && List.contains (x, y - 1) borderClockwise |> not then yield (x, y - 1)
        if x < (Array2D.length1 maze - 1) && List.contains (x + 1, y) borderClockwise |> not then yield (x + 1, y)
        if y < (Array2D.length2 maze - 1) && List.contains (x, y + 1) borderClockwise |> not then yield (x, y + 1)
    ]

let rec countArea maze queue processed area =
    match queue with
    | [] -> area
    | h :: ts -> if Set.contains h processed
                 then countArea maze ts processed area
                 else let nts = neighborTiles maze h |> List.except (Set.toList processed)
                      countArea maze (List.append nts ts) (Set.add h processed) (Set.add h area)

let areaTiles = countArea maze borderTiles Set.empty<int * int> Set.empty<int * int>
let result2 = areaTiles |> Set.count
