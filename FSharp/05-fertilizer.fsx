open System
open System.IO

type Range = {
    sourceRangeStart: int64
    destinationRangeStart: int64
    rangeLength: int64
}

type Conversion = Range list

type Almanac = {
    seeds: int64 list
    conversions: Conversion list
}

let convert conversion num =
    let range = conversion
                |> List.tryFind (fun r -> num >= r.sourceRangeStart && num < r.sourceRangeStart + r.rangeLength)
    match range with
    | Some range -> range.destinationRangeStart + (num - range.sourceRangeStart)
    | None -> num

let parseAlmanac (lines: string list) =
    let seeds = (List.head lines).Split(": ").[1].Split(' ')
                |> Array.map Int64.Parse
                |> List.ofArray
    
    let parseRange (line: string) =
        let nums = line.Split ' '
        {
            sourceRangeStart = nums.[1] |> Int64.Parse
            destinationRangeStart = nums.[0] |> Int64.Parse
            rangeLength = nums.[2] |> Int64.Parse
        }
    
    let rec parseConversions (lines: string list) acc =
        match lines with
        | [] -> acc |> List.rev
        | lines -> let ranges = lines
                                |> List.skip 2
                                |> List.takeWhile (fun l -> l <> "")
                                |> List.map parseRange
                   parseConversions (lines |> List.skip (2 + (List.length ranges))) (ranges :: acc)
    {
        seeds = seeds
        conversions = parseConversions (lines |> List.skip 1) []
    }

let almanac = File.ReadAllLines "FSharp/05-fertilizer-input.txt"
              |> List.ofArray
              |> parseAlmanac

let rec convertAll conversions num =
    match conversions with
    | [] -> num
    | h :: t -> convertAll t (convert h num)

let result1 = almanac.seeds
              |> List.map (fun s -> convertAll almanac.conversions s)
              |> List.min
