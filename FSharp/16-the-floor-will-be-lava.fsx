open System
open System.IO

let lines = File.ReadAllLines "FSharp/16-the-floor-will-be-lava-input.txt"

let cave = Array2D.init
               lines.[0].Length
               lines.Length
               (fun x y -> lines.[y].[x])

type Direction = Up | Down | Left | Right

type Ray = {
    pos: int * int
    dir: Direction
}

let move dir (x, y) = match dir with
                      | Up -> (x, y - 1)
                      | Down -> (x, y + 1)
                      | Left -> (x - 1, y)
                      | Right -> (x + 1, y)

let isInBounds (cave: char[,]) (x, y) =
    x >= 0 && x < Array2D.length1 cave && y >= 0 && y < Array2D.length2 cave

let step cave ray =
    let (nx, ny) = move ray.dir ray.pos
    if isInBounds cave (nx, ny)
    then match cave.[nx, ny] with
         | '.' -> [ { pos = (nx, ny); dir = ray.dir } ]
         | '/' -> let newDir = match (ray.dir) with
                               | Up -> Right
                               | Down -> Left
                               | Left -> Down
                               | Right -> Up
                  [ { pos = (nx, ny); dir = newDir } ]
         | '\\' -> let newDir = match (ray.dir) with
                                | Up -> Left
                                | Down -> Right
                                | Left -> Up
                                | Right -> Down
                   [ { pos = (nx, ny); dir = newDir } ]
         | '|' -> match ray.dir with
                  | Up | Down -> [ { pos = (nx, ny); dir = ray.dir } ]
                  | Left | Right -> [ { pos = (nx, ny); dir = Up }; { pos = (nx, ny); dir = Down } ]
         | '-' -> match ray.dir with
                  | Left | Right -> [ { pos = (nx, ny); dir = ray.dir } ]
                  | Up | Down -> [ { pos = (nx, ny); dir = Left }; { pos = (nx, ny); dir = Right } ]
         | _ -> failwith "Unexpected input"
    else []

let stepAllRays cave rays =
    rays |> List.collect (fun r -> step cave r)

let rec progressRays cave rays energized handledRays cnt =
    // printfn "Running #%d, ray count: %d, first ray: %A" cnt (List.length rays) (List.head rays)
    let energized = Set.union energized (rays |> List.map (fun r -> r.pos) |> Set.ofList)
    let handledRays = Set.union handledRays (rays |> Set.ofList)

    if cnt <= 0 || (List.isEmpty rays)
    then energized
    else let rays = stepAllRays cave rays
         let rays = List.except (Set.toList handledRays) rays
        //  printfn "Energized count: %d" (Set.count energized)

         progressRays cave rays energized handledRays (cnt - 1)

let energized = progressRays cave [ { pos = (-1, 0); dir = Right } ] Set.empty<int * int> Set.empty<Ray> 10000

let result1 = (Set.count energized) - 1

// Part 2
let calculateEnergized
