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

let areMirroredAt i items =
    if i < 0 || i >= Array.length items - 1
    then false
    else let rec areMirroredAt i1 i2 items =
             if i1 < 0 || i2 > (Array.length items - 1)
             then true
             else if items.[i1] <> items.[i2]
             then false
             else areMirroredAt (i1 - 1) (i2 + 1) items

         areMirroredAt i (i + 1) items

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
             |> List.tryFind (fun y -> areMirroredAt y pattern.rows)

    match rm with
    | Some rm -> rm, true
    | None -> let cm = [0..(Array.length pattern.columns - 1)]
                       |> List.find (fun x -> areMirroredAt x pattern.columns)
              cm, false

let reflectionScore (i, isRow) =
    if isRow
    then (i + 1) * 100
    else i + 1

let rs = patterns |> List.map findReflection

let result1 = rs |> List.map reflectionScore |> List.sum

// Part 2
let bitsSet i =
    let rec bitsSet i cnt =
        if i = 0
        then cnt
        else bitsSet (i >>> 1) (cnt + (i &&& 1))
    
    bitsSet i 0

let areMirroredAt2 i items =
    if i < 0 || i >= Array.length items - 1
    then false
    else let rec areMirroredAt2 i1 i2 items foundDiff =
             if i1 < 0 || i2 > (Array.length items - 1)
             then foundDiff
             else if items.[i1] = items.[i2]
             then areMirroredAt2 (i1 - 1) (i2 + 1) items foundDiff
             else if foundDiff
             then false
             else if items.[i1] ^^^ items.[i2] |> bitsSet = 1
             then areMirroredAt2 (i1 - 1) (i2 + 1) items true
             else false

         areMirroredAt2 i (i + 1) items false

let findReflection2 pattern =
    printfn "Finding reflection for pattern #%d" pattern.index
    let rm = [0..(Array.length pattern.rows - 1)]
             |> List.tryFind (fun y -> areMirroredAt2 y pattern.rows)

    match rm with
    | Some rm -> rm, true
    | None -> let cm = [0..(Array.length pattern.columns - 1)]
                       |> List.find (fun x -> areMirroredAt2 x pattern.columns)
              cm, false

let rs2 = patterns |> List.map findReflection2

let result2 = rs2 |> List.map reflectionScore |> List.sum