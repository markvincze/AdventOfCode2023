open System
open System.IO

type Dir = Up | Down | Left | Right

type Dig = {
    dir : Dir
    length : int
    // longDir : Dir
    // longLength : int
}

type BorderType =
    | Horizontal
    | Vertical
    | BottomLeft
    | BottomRight
    | TopLeft
    | TopRight

type Corner = BorderType * int * int
type Vertical = int * int * int

let parseDir = function
               | "U" -> Up
               | "D" -> Down
               | "L" -> Left
               | "R" -> Right
               | _ -> failwith "Invalid input"

let parse (line : string) =
    let [|dir; length; color |] = line.Split ' '

    {
        dir = parseDir dir
        length = Int32.Parse length
        // length = Int32.Parse(color.Substring(2, 5), Globalization.NumberStyles.HexNumber);
        // dir = match color[7] with
        //           | '0' -> Right
        //           | '1' -> Down
        //           | '2' -> Left
        //           | '3' -> Up
        //           | _ -> failwith "Invalid input"
    }

let digs = File.ReadAllLines "FSharp/18-lavaduct-lagoon-input.txt"
           |> Array.map parse
           |> List.ofArray

let rec buildMap (x, y) digs m =
    match digs with
    | [] -> m
    | h :: t ->
        let ps = match h.dir with
                 | Up -> seq { 1..(h.length) }
                         |> Seq.map (fun i -> (x, y - i))
                 | Down -> seq { 1..(h.length) }
                           |> Seq.map (fun i -> (x, y + i))
                 | Left -> seq { 1..(h.length) }
                           |> Seq.map (fun i -> (x - i, y))
                 | Right -> seq { 1..(h.length) }
                            |> Seq.map (fun i -> (x + i, y))

        let m = Seq.fold
                    (fun m p -> Map.add p true m)
                    m
                    ps

        buildMap (Seq.last ps) t m

let rec buildMap2 (x, y) digs corners verticals firstDir =
    match digs with
    | [] -> corners, verticals
    | h :: t ->
        let (nextX, nextY) = match h.dir with
                             | Up -> (x, y - h.length)
                             | Down -> (x, y + h.length)
                             | Left -> (x - h.length, y)
                             | Right -> (x + h.length, y)

        let verticals = match h.dir with
                        | Up -> (x, y - h.length, h.length) :: verticals
                        | Down -> (x, y, y + h.length) :: verticals
                        | Left | Right -> verticals
 
        let nextDir = match t with
                      | [] -> firstDir
                      | next :: _ -> next.dir

        let newCornerType = match h.dir, nextDir with
                            | Up, Right | Left, Down -> TopLeft
                            | Up, Left | Right, Down -> TopRight
                            | Down, Left | Right, Up -> BottomRight
                            | Down, Right | Left, Up -> BottomLeft
                            | _ -> failwith "Unexpected scenario"

        buildMap2 (nextX, nextY) t ((newCornerType, nextX, nextY) :: corners) verticals firstDir

let digMap = buildMap (0, 0) digs (Map.add (0, 0) true Map.empty<(int * int), bool>)

let minX = Map.keys digMap |> Seq.map fst |> Seq.min
let maxX = Map.keys digMap |> Seq.map fst |> Seq.max
let minY = Map.keys digMap |> Seq.map snd |> Seq.min
let maxY = Map.keys digMap |> Seq.map snd |> Seq.max

let borderType digMap (x, y) =
    let dug (x, y) = Map.containsKey (x, y) digMap
    if dug (x, y)
    then if dug (x - 1, y) && dug (x + 1, y) then Some Horizontal
         else if dug (x, y - 1) && dug (x, y + 1) then Some Vertical
         else if dug (x, y - 1) && dug (x + 1, y) then Some BottomLeft
         else if dug (x, y - 1) && dug (x - 1, y) then Some BottomRight
         else if dug (x, y + 1) && dug (x - 1, y) then Some TopRight
         else if dug (x, y + 1) && dug (x + 1, y) then Some TopLeft
         else failwith "Unexpected pattern"
    else None

// let isInside digMap (x, y) =
//     let rec transCount (x, y) digMap maxX prevCorner acc =
//         if x > maxX then acc
//         else match borderType digMap (x, y), prevCorner with
//              | None, _ -> transCount (x + 1, y) digMap maxX prevCorner acc
//              | Some Horizontal, _ -> transCount (x + 1, y) digMap maxX prevCorner acc
//              | Some Vertical, _ -> transCount (x + 1, y) digMap maxX prevCorner (acc + 1)
//              | Some BottomLeft, _ -> transCount (x + 1, y) digMap maxX BottomLeft acc
//              | Some BottomRight, BottomLeft -> transCount (x + 1, y) digMap maxX prevCorner acc
//              | Some TopRight, BottomLeft -> transCount (x + 1, y) digMap maxX prevCorner (acc + 1)
//              | Some TopLeft, _ -> transCount (x + 1, y) digMap maxX TopLeft acc
//              | Some TopRight, TopLeft -> transCount (x + 1, y) digMap maxX prevCorner acc
//              | Some BottomRight, TopLeft -> transCount (x + 1, y) digMap maxX prevCorner (acc + 1)
//              | _ -> failwith "Unexpected pattern"
    
//     Map.containsKey (x, y) digMap || transCount (x, y) digMap maxX BottomLeft 0 % 2 = 1

let printDigMap1 digMap =
    for y in minY-2..maxY+2 do
        for x in minX-2..maxX+2 do
            printf (if Map.containsKey (x, y) digMap then "X" else ".")
        printfn ""

// let printDigMap2 digMap =
//     for y in minY-2..maxY+2 do
//         for x in minX-2..maxX+2 do
//             printf (if isInside digMap (x, y) then "X" else ".")
//         printfn ""

let insideCount digMap row =
    let rec insideCount digMap row x isInside prevCorner prevCornerInside acc =
        // if row % 10 = 0 && x = 0 then printfn "Processing row %d" row
        if x = 0 then printfn "Processing row %d" row

        if x > maxX
        then acc
        else match borderType digMap (x, row), isInside, prevCorner, prevCornerInside with
             | None, false, _, _ -> insideCount digMap row (x + 1) false prevCorner prevCornerInside acc
             | None, true, _, _ -> insideCount digMap row (x + 1) true prevCorner prevCornerInside (acc + 1)
             | Some Horizontal, true, _, _ -> insideCount digMap row (x + 1) true prevCorner prevCornerInside (acc + 1)
             | Some Vertical, false, _, _ -> insideCount digMap row (x + 1) true prevCorner prevCornerInside (acc + 1)
             | Some Vertical, true, _, _ -> insideCount digMap row (x + 1) false prevCorner prevCornerInside (acc + 1)
             | Some BottomLeft, false, _, _ -> insideCount digMap row (x + 1) true BottomLeft false (acc + 1)
             | Some BottomLeft, true, _, _ -> insideCount digMap row (x + 1) true BottomLeft true (acc + 1)
             | Some BottomRight, true, BottomLeft, true -> insideCount digMap row (x + 1) true BottomRight true (acc + 1)
             | Some BottomRight, true, BottomLeft, false -> insideCount digMap row (x + 1) false BottomRight true (acc + 1)
             | Some BottomRight, true, TopLeft, true -> insideCount digMap row (x + 1) false BottomRight true (acc + 1)
             | Some BottomRight, true, TopLeft, false -> insideCount digMap row (x + 1) true BottomRight true (acc + 1)
             | Some TopLeft, false, _, _ -> insideCount digMap row (x + 1) true TopLeft false (acc + 1)
             | Some TopLeft, true, _, _ -> insideCount digMap row (x + 1) true TopLeft true (acc + 1)
             | Some TopRight, true, TopLeft, true -> insideCount digMap row (x + 1) true TopRight true (acc + 1)
             | Some TopRight, true, TopLeft, false -> insideCount digMap row (x + 1) false TopRight true (acc + 1)
             | Some TopRight, true, BottomLeft, true -> insideCount digMap row (x + 1) false TopRight true (acc + 1)
             | Some TopRight, true, BottomLeft, false -> insideCount digMap row (x + 1) true TopRight true (acc + 1)
             | Some bt, i, prevCorner, prevCornerInside -> failwith $"Unexpected scenario: Row: {row}, X: {x}, Border: {bt}, IsInside: {i}, PrevCorner: {prevCorner}, PrevCornerInside: {prevCornerInside}"

    insideCount digMap row minX false BottomLeft false 0

let cornersPerRow = Map.empty<int, 

// printDigMap1 digMap
// printfn ""
// printDigMap2 digMap

// let result1 =
//     seq { for y in minY-1..maxY+1 do
//             for x in minX-1..maxX+1 do
//                 yield (x, y) }
//     |> Seq.filter (fun p -> isInside digMap p)
//     |> Seq.length

let result1 =
    seq { minY-1..maxY }
    |> Seq.map (fun row -> insideCount digMap row)
    |> Seq.sum
