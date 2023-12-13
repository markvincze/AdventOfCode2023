open System
open System.IO

type Pattern = {
    index: int
    rows: int array
    columns: int array
}

let parse (lines: string list) index =
    let rows = lines
               |> List.map (fun l -> l.Replace('#', '1').Replace('.', '0'))
               |> List.map (fun l -> Convert.ToInt32(l, 2))

    let columns = [0..(lines.[0].Length - 1)]
                  |> List.map (fun x -> [0..(List.length lines - 1)] |> List.map (fun y -> lines.[y].[x]) |> String.Concat)
                  |> List.map (fun l -> l.Replace('#', '1').Replace('.', '0'))
                  |> List.map (fun l -> Convert.ToInt32(l, 2))
    
    {
        index = index
        rows = rows |> Array.ofList
        columns = columns |> Array.ofList
    }

let ls = File.ReadAllLines "FSharp/13-point-of-incidence-input.txt"
         |> Array.takeWhile(fun l -> l <> "")

let areRowsMirroredAt y rows = 
    if y < 0 || y >= Array.length rows - 1
    then false
    else let rec areRowsMirroredAt y1 y2 rows =
             if y1 < 0 || y2 > (Array.length rows - 1)
             then true
             else if rows.[y1] <> rows.[y2]
             then false
             else areRowsMirroredAt (y1 - 1) (y2 + 1) rows
         
         areRowsMirroredAt y (y + 1) rows

let areColumnsMirroredAt x columns = 
    if x < 0 || x >= Array.length columns - 1
    then false
    else let rec areColumnsMirroredAt x1 x2 columns =
             if x1 < 0 || x2 > (Array.length columns - 1)
             then true
             else if columns.[x1] <> columns.[x2]
             then false
             else areColumnsMirroredAt (x1 - 1) (x2 + 1) columns
         
         areColumnsMirroredAt x (x + 1) columns

let parsePatterns lines =
    let rec parsePatterns lines acc =
        let ls = lines |> List.takeWhile (fun l -> l <> "")
        if List.length lines > List.length ls
        then parsePatterns (List.skip (List.length ls + 1) lines) (parse ls (List.length acc) :: acc)
        else (parse ls (List.length acc) :: acc) |> List.rev
    
    parsePatterns lines []

let patterns = File.ReadAllLines "FSharp/13-point-of-incidence-input.txt"
               |> List.ofArray
               |> parsePatterns

let findReflection pattern =
    printfn "Finding reflection for pattern #%d" pattern.index
    let rm = [0..(Array.length pattern.rows - 1)]
             |> List.tryFind (fun y -> areRowsMirroredAt y pattern.rows)
 
    match rm with
    | Some rm -> rm, true
    | None -> let cm = [0..(Array.length pattern.columns - 1)]
                       |> List.find (fun x -> areColumnsMirroredAt x pattern.columns)
              cm, false

let reflectionScore (i, isRow) =
    if isRow
    then (i + 1) * 100
    else i + 1

let rs = patterns |> List.map findReflection

let result1 = rs |> List.map reflectionScore |> List.sum