open System
open System.IO

type Set = {
    Red: int
    Green: int
    Blue: int
}

type Game = {
    Id: int
    Sets: Set list
}

let parseSet (str: string) =
    let cubes = str.Split(',', StringSplitOptions.TrimEntries)
    let getColorValue (name: string) =
        match cubes |> Array.tryFind (fun s -> s.EndsWith name) with
        | Some s -> Int32.Parse (s.Substring(0, s.IndexOf(" ")))
        | None -> 0

    {
        Red = getColorValue "red"
        Green = getColorValue "green" 
        Blue = getColorValue "blue"
    }

let parseGame (line: string) =
    let sections = line.Split ':'
    {
        Id = sections.[0].Substring(sections.[0].IndexOf " ") |> Int32.Parse
        Sets = sections.[1].Split ';'
               |> Array.map parseSet
               |> List.ofArray
    }

let fitsLimit red green blue game =
    game.Sets
    |> List.forall (fun s -> s.Red <= red && s.Green <= green && s.Blue <= blue)

let result1 = File.ReadAllLines "FSharp/02-cube-conundrum-input.txt"
              |> Array.map parseGame
              |> Array.filter (fitsLimit 12 13 14)
              |> Array.sumBy (fun game -> game.Id)

let getPowerOfFewest game =
    (game.Sets |> List.map (fun s -> s.Red) |> List.max) *
    (game.Sets |> List.map (fun s -> s.Blue) |> List.max) *
    (game.Sets |> List.map (fun s -> s.Green) |> List.max)

let result2 = File.ReadAllLines "FSharp/02-cube-conundrum-input.txt"
              |> Array.map parseGame
              |> Array.sumBy getPowerOfFewest
