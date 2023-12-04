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

let rec readSymbol x y (schematic: char[,]) digits hasAnyAdjacentSymbol =
    if x >= Array2D.length1 schematic || Char.IsDigit (schematic.[x, y]) |> not
    then if hasAnyAdjacentSymbol
         then digits |> List.mapi (fun i d -> pown 10 i * d) |> List.sum |> Some
         else None
    else readSymbol (x + 1) y schematic ((schematic.[x, y] |> string |> Int32.Parse) :: digits) (hasAnyAdjacentSymbol || hasAdjacentSymbol x y schematic)

let getPartNumber x y (schematic: char[,]) =
    let c = schematic.[x, y]

    if Char.IsDigit c |> not then None
    else if x > 0 && Char.IsDigit (schematic.[x - 1, y]) then None
    else readSymbol x y schematic [] false

let result1 =
    [ for x in 0..(Array2D.length1 schematic - 1) do
          for y in 0..(Array2D.length2 schematic - 1) -> (x, y)]
    |> List.choose (fun (x, y) -> getPartNumber x y schematic)
    |> List.sum