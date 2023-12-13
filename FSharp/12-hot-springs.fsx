open System
open System.IO
open System.Linq

type Line = {
    line: string
    damagedGroups: int list
    unknownPositions: int list
}

let getUnknownPositions (line: string) =
    line
    |> Seq.indexed
    |> Seq.filter (fun (i, c) -> c = '?')
    |> Seq.map fst
    |> List.ofSeq

let parseLine (line: string) = 
    let [| l; g |] = line.Split ' '

    {
        line = l
        damagedGroups = g.Split ',' |> Array.map Int32.Parse |> List.ofArray
        unknownPositions = getUnknownPositions l
    }

let lines = File.ReadAllLines "FSharp/12-hot-springs-input.txt"
            |> List.ofArray
            |> List.map parseLine

let getDamagedGroups line extraDamagedPositions =
    let rec getDamagedGroups (line: string) extraDamagedPositions acc currentGroupSize index =
        if index >= line.Length
        then if currentGroupSize > 0
             then (currentGroupSize :: acc) |> List.rev
             else acc |> List.rev
        else let prev = if index > 0
                        then line.[index - 1] = '.' || (line.[index - 1] = '?' && extraDamagedPositions |> List.contains (index - 1) |> not)
                        else true
             let current = line.[index] = '.' || (line.[index] = '?' && extraDamagedPositions |> List.contains (index) |> not)
             match prev, current with
             | true, true -> getDamagedGroups line extraDamagedPositions acc 0 (index + 1)
             | false, false -> getDamagedGroups line extraDamagedPositions acc (currentGroupSize + 1) (index + 1)
             | true, false -> getDamagedGroups line extraDamagedPositions acc 1 (index + 1)
             | false, true -> getDamagedGroups line extraDamagedPositions (currentGroupSize :: acc) 0 (index + 1)
    
    getDamagedGroups line extraDamagedPositions [] 0 0

let dgs = getDamagedGroups "????.######..#####." [ 2; 3 ]

let rec choose n (l: list<int>) =
    match n, l with
    | 0, _ -> [[]] |> Seq.ofList
    | _, [] -> Seq.empty<list<int>>
    | n, h :: t -> let withH = choose (n - 1) t |> Seq.map (fun xs -> h :: xs)
                   let withoutH = choose n t
                   Seq.append withH withoutH


let possibleArrangements line =
    let extraDamagedPositionCount = (line.damagedGroups |> List.sum) - (line.line |> Seq.filter (fun c -> c = '#') |> Seq.length)
    choose extraDamagedPositionCount line.unknownPositions
    |> Seq.filter (fun extraDamagedPositions -> getDamagedGroups line.line extraDamagedPositions = line.damagedGroups)
    |> Seq.length

let results = lines
              |> List.map possibleArrangements

let result1 = results |> List.sum

let lines2 = lines
             |> List.map (fun line -> 
                let extendedLine = String.Join("?", Enumerable.Repeat(line.line, 5))
                {
                    line = extendedLine
                    damagedGroups = [0..4] |> List.collect (fun _ -> line.damagedGroups)
                    unknownPositions = getUnknownPositions extendedLine
                })

let results2 = lines2
               |> List.map possibleArrangements

let result2 = results2 |> List.sum
