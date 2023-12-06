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

let getAllSeeds seeds =
    seq {
        for [ start; length ] in seeds |> List.chunkBySize 2 do
            yield! seq { start..(start + length - 1L) }
    }

let result2 = almanac.seeds
              |> getAllSeeds
              |> Seq.map (fun s -> convertAll almanac.conversions s)
              |> Seq.min

let printRange range = match range with
                       | None -> "None"
                       | Some range -> sprintf "%d %d %d" range.destinationRangeStart range.sourceRangeStart range.rangeLength

let combineRanges topRanges bottomRanges =
    let rec combineRanges topRanges bottomRanges currentTop currentBottom acc runIndex =
        let nextRange, isNextTop =
            match (List.tryHead topRanges), (List.tryHead bottomRanges) with
            | Some nt, Some nb when nt.destinationRangeStart <= nb.sourceRangeStart -> Some nt, true
            | Some nt, Some nb when nt.destinationRangeStart > nb.sourceRangeStart -> Some nb, false
            | Some nt, None -> Some nt, true
            | None, Some nb -> Some nb, false
            | None, None -> None, false

        let newRange =
            match currentTop, currentBottom, nextRange, isNextTop with
            | None, None, _, _ -> None
            | Some currentTop, Some currentBottom, Some nextRange, true ->
                if currentTop.destinationRangeStart <= currentBottom.sourceRangeStart
                then { sourceRangeStart = currentTop.sourceRangeStart + (currentBottom.sourceRangeStart - currentTop.destinationRangeStart)
                       destinationRangeStart = currentBottom.destinationRangeStart
                       rangeLength = nextRange.destinationRangeStart - currentBottom.sourceRangeStart } |> Some
                else { sourceRangeStart = currentTop.sourceRangeStart
                       destinationRangeStart = currentBottom.destinationRangeStart + (currentTop.destinationRangeStart - currentBottom.sourceRangeStart)
                       rangeLength = nextRange.destinationRangeStart - currentTop.destinationRangeStart } |> Some
            | Some currentTop, Some currentBottom, Some nextRange, false ->
                if currentTop.destinationRangeStart <= currentBottom.sourceRangeStart
                then { sourceRangeStart = currentTop.sourceRangeStart + (currentBottom.sourceRangeStart - currentTop.destinationRangeStart)
                       destinationRangeStart = currentBottom.destinationRangeStart
                       rangeLength = nextRange.sourceRangeStart - currentBottom.sourceRangeStart } |> Some
                else { sourceRangeStart = currentTop.sourceRangeStart
                       destinationRangeStart = currentBottom.destinationRangeStart + (currentTop.destinationRangeStart - currentBottom.sourceRangeStart)
                       rangeLength = nextRange.sourceRangeStart - currentTop.destinationRangeStart } |> Some
            | None, Some currentBottom, Some nextRange, true ->
                { sourceRangeStart = currentBottom.sourceRangeStart
                  destinationRangeStart = currentBottom.destinationRangeStart
                  rangeLength = nextRange.destinationRangeStart - currentBottom.sourceRangeStart } |> Some
            | None, Some currentBottom, Some nextRange, false ->
                { sourceRangeStart = currentBottom.sourceRangeStart
                  destinationRangeStart = currentBottom.destinationRangeStart
                  rangeLength = nextRange.sourceRangeStart - currentBottom.sourceRangeStart } |> Some
            | Some currentTop, None, Some nextRange, true ->
                { sourceRangeStart = currentTop.sourceRangeStart
                  destinationRangeStart = currentTop.destinationRangeStart
                  rangeLength = nextRange.sourceRangeStart - currentTop.sourceRangeStart } |> Some
            | Some currentTop, None, Some nextRange, false ->
                { sourceRangeStart = currentTop.sourceRangeStart
                  destinationRangeStart = currentTop.destinationRangeStart
                  rangeLength = nextRange.sourceRangeStart - currentTop.destinationRangeStart } |> Some
            | Some currentTop, None, _, _ -> currentTop |> Some
            | _ -> None

        printfn "Run %d" runIndex
        printfn "currentTop: %s" (printRange currentTop)
        printfn "currentBottom: %s" (printRange currentBottom)
        printfn "nextRange: %s, %s" (if isNextTop then "Top" else "Bottom") (printRange nextRange)
        printfn "newRange: %s" (printRange newRange)
        printfn ""

        match nextRange, isNextTop with
        | None, _ -> acc |> List.choose id |> List.rev
        | Some nextRange, true -> combineRanges (List.tail topRanges) bottomRanges (Some nextRange) currentBottom (newRange :: acc) (runIndex + 1)
        | Some nextRange, false -> combineRanges topRanges (List.tail bottomRanges) currentTop (Some nextRange) (newRange :: acc) (runIndex + 1)

    combineRanges topRanges bottomRanges None None [] 1

let c0 = almanac.conversions |> List.item 0 |> List.sortBy (fun r -> r.destinationRangeStart)
let c1 = almanac.conversions |> List.item 1 |> List.sortBy (fun r -> r.sourceRangeStart)

// let combined = combineRanges c0 c1
let combined = combineRanges c0 c1 |> List.sortBy (fun r -> r.sourceRangeStart)

printfn "Combined"
combined |> List.iter (fun r -> printfn "%s" (printRange (Some r)))
