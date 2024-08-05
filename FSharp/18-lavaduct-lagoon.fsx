open System
open System.IO

type Dir = Up | Down | Left | Right

type Dig = {
    dir : Dir
    length : int
}

type BorderType =
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
        // Part 1
        // dir = parseDir dir
        // length = Int32.Parse length

        // Part 2
        length = Int32.Parse(color.Substring(2, 5), Globalization.NumberStyles.HexNumber);
        dir = match color[7] with
                  | '0' -> Right
                  | '1' -> Down
                  | '2' -> Left
                  | '3' -> Up
                  | _ -> failwith "Invalid input"
    }

let digs = File.ReadAllLines "FSharp/18-lavaduct-lagoon-input.txt"
           |> Array.map parse
           |> List.ofArray

let rec buildMap (x, y) digs corners verticals firstDir =
    match digs with
    | [] -> corners, verticals
    | h :: t ->
        let (nextX, nextY) = match h.dir with
                             | Up -> (x, y - h.length)
                             | Down -> (x, y + h.length)
                             | Left -> (x - h.length, y)
                             | Right -> (x + h.length, y)

        let verticals = match h.dir with
                        | Up -> (x, y - h.length + 1, h.length - 1) :: verticals
                        | Down -> (x, y + 1, h.length - 1) :: verticals
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

        buildMap (nextX, nextY) t ((newCornerType, nextX, nextY) :: corners) verticals firstDir

let corners, verticals = buildMap (0, 0) digs [] [] (List.head digs).dir

let minY = corners |> List.map (fun (_, _, y) -> y) |> List.min
let maxY = corners |> List.map (fun (_, _, y) -> y) |> List.max

let insideCount corners verticals row =
    let rec insideCount borders insideStartX isInside prevCorner prevCornerInside acc =
        match borders with
        | [] -> acc
        | h :: t -> //printfn $"Processing: Row: {row}, X: {snd h}, Border: {fst h}, insideStartX: {insideStartX}, IsInside: {isInside}, PrevCorner: {prevCorner}, PrevCornerInside: {prevCornerInside}"
                    match h, isInside, prevCorner, prevCornerInside with
                    | (Vertical, x), false, _, _ -> insideCount t x true prevCorner prevCornerInside acc
                    | (Vertical, x), true, _, _ -> insideCount t insideStartX false prevCorner prevCornerInside (acc + (x - insideStartX + 1 |> int64))
                    | (BottomLeft, x), false, _, _ -> insideCount t x true BottomLeft false acc
                    | (BottomLeft, x), true, _, _ -> insideCount t insideStartX true BottomLeft true acc
                    | (BottomRight, x), true, BottomLeft, true -> insideCount t insideStartX true BottomRight true acc
                    | (BottomRight, x), true, BottomLeft, false -> insideCount t insideStartX false BottomRight true (acc + (x - insideStartX + 1 |> int64))
                    | (BottomRight, x), true, TopLeft, true -> insideCount t insideStartX false BottomRight true (acc + (x - insideStartX + 1 |> int64))
                    | (BottomRight, x), true, TopLeft, false -> insideCount t insideStartX true BottomRight true acc
                    | (TopLeft, x), false, _, _ -> insideCount t x true TopLeft false acc
                    | (TopLeft, x), true, _, _ -> insideCount t insideStartX true TopLeft true acc
                    | (TopRight, x), true, TopLeft, true -> insideCount t insideStartX true TopRight true acc
                    | (TopRight, x), true, TopLeft, false -> insideCount t insideStartX false TopRight true (acc + (x - insideStartX + 1 |> int64))
                    | (TopRight, x), true, BottomLeft, true -> insideCount t insideStartX false TopRight true (acc + (x - insideStartX + 1 |> int64))
                    | (TopRight, x), true, BottomLeft, false -> insideCount t insideStartX true TopRight true acc
                    | (bt, x), i, prevCorner, prevCornerInside -> failwith $"Unexpected scenario: Row: {row}, X: {x}, Border: {bt}, IsInside: {i}, PrevCorner: {prevCorner}, PrevCornerInside: {prevCornerInside}"

    let cornersInRow = corners
                       |> List.filter (fun (_, _, y) -> y = row)
                       |> List.map (fun  (bt, x, _) -> bt, x)
    let verticalsInRow = verticals
                         |> List.filter (fun (_, y, len) -> row >= y && row < y + len)
                         |> List.map (fun (x, _, _) -> Vertical, x)

    let borders = List.append cornersInRow verticalsInRow |> List.sortBy snd

    insideCount borders 0 false BottomLeft false 0L

let result =
    seq { minY-1..maxY }
    |> Seq.map (fun row -> insideCount corners verticals row)
    |> Seq.sum
