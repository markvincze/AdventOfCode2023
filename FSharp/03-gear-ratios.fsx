open System
open System.IO

let lines = File.ReadAllLines "FSharp/03-gear-ratios-input.txt"

let schematic =
    Array2D.init
        (lines.[0].Length)
        lines.Length
        (fun x y -> lines.[y].[x])

let isDigit x y schematic =
    x >= 0 && x < Array2D.length1 schematic && y >= 0 && y < Array2D.length2 schematic && Char.IsDigit (schematic.[x, y])

let isSymbol x y schematic =
    x >= 0 && x < Array2D.length1 schematic && y >= 0 && y < Array2D.length2 schematic && Char.IsDigit (schematic.[x, y]) |> not && schematic.[x, y] <> '.'

let hasAdjacentSymbol x y schematic =
    isSymbol (x - 1) (y - 1) schematic ||
    isSymbol (x) (y - 1) schematic ||
    isSymbol (x + 1) (y - 1) schematic ||
    isSymbol (x + 1) (y) schematic ||
    isSymbol (x + 1) (y + 1) schematic ||
    isSymbol (x) (y + 1) schematic ||
    isSymbol (x - 1) (y + 1) schematic ||
    isSymbol (x - 1) (y) schematic

let getAdjacentSymbols x y schematic =
    [ if isSymbol (x - 1) (y - 1) schematic then yield (x - 1), (y - 1)
      if isSymbol (x) (y - 1) schematic then yield (x), (y - 1)
      if isSymbol (x + 1) (y - 1) schematic then yield (x + 1), (y - 1)
      if isSymbol (x + 1) (y) schematic then yield (x + 1), (y)
      if isSymbol (x + 1) (y + 1) schematic then yield (x + 1), (y + 1)
      if isSymbol (x) (y + 1) schematic then yield (x), (y + 1)
      if isSymbol (x - 1) (y + 1) schematic then yield (x - 1), (y + 1)
      if isSymbol (x - 1) (y) schematic then  yield (x - 1), (y) ] |> Set.ofList

let rec readSymbol x y (schematic: char[,]) digits adjacentSymbols =
    if x >= Array2D.length1 schematic || Char.IsDigit (schematic.[x, y]) |> not
    then if Set.isEmpty adjacentSymbols |> not
         then (digits |> List.mapi (fun i d -> pown 10 i * d) |> List.sum, adjacentSymbols) |> Some
         else None
    else let adjacentSymbols = getAdjacentSymbols x y schematic |> Set.union adjacentSymbols
         readSymbol (x + 1) y schematic ((schematic.[x, y] |> string |> Int32.Parse) :: digits) adjacentSymbols

let getPartNumber x y (schematic: char[,]) =
    let c = schematic.[x, y]

    if Char.IsDigit c |> not then None
    else if x > 0 && Char.IsDigit (schematic.[x - 1, y]) then None
    else readSymbol x y schematic [] Set.empty

let result1 =
    [ for x in 0..(Array2D.length1 schematic - 1) do
          for y in 0..(Array2D.length2 schematic - 1) -> (x, y)]
    |> List.choose (fun (x, y) -> getPartNumber x y schematic)
    |> List.sumBy fst

let partNumbers = 
    [ for x in 0..(Array2D.length1 schematic - 1) do
          for y in 0..(Array2D.length2 schematic - 1) -> (x, y)]
    |> List.choose (fun (x, y) -> getPartNumber x y schematic)

let gears =
    List.fold
        (fun gears partNumber ->
            Set.fold
                (fun gs (x, y) ->
                    if schematic.[x, y] = '*'
                    then gs
                         |> Map.change
                                (x, y)
                                (fun ps -> match ps with
                                           | None -> [ fst partNumber ] |> Some
                                           | Some ps -> fst partNumber :: ps |> Some)
                    else gs)
                gears
                (snd partNumber))
        Map.empty<int * int, int list> 
        partNumbers

let result2 = gears
              |> Map.filter (fun _ ps -> List.length ps = 2)
              |> Map.values
              |> Seq.sumBy (fun ps -> ps.[0] * ps.[1])
