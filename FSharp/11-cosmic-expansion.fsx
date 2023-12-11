open System
open System.IO

let lines = File.ReadAllLines "FSharp/11-cosmic-expansion-input.txt"
let space = Array2D.init
                lines.[0].Length
                lines.Length
                (fun x y -> lines.[x].[y])

let emptyColumns = [ 0..(Array2D.length1 space - 1) ]
                   |> List.filter (fun x -> [ 0..(Array2D.length2 space - 1) ]
                                            |> List.forall (fun y -> space.[x, y] = '.'))

let emptyRows = [ 0..(Array2D.length2 space - 1) ]
                |> List.filter (fun y -> [ 0..(Array2D.length1 space - 1) ]
                                         |> List.forall (fun x -> space.[x, y] = '.'))

let dist emptyColumns emptyRows exp (x1, y1) (x2, y2) =
    let simpleDist = (abs (x1 - x2)) + (abs (y1 - y2))
    let emptyColumnsBetween = emptyColumns |> List.filter (fun x -> x > (min x1 x2) && x < (max x1 x2)) |> List.length
    let emptyRowsBetween = emptyRows |> List.filter (fun y -> y > (min y1 y2) && y < (max y1 y2)) |> List.length

    (int64 simpleDist) + ((int64 emptyColumnsBetween) * exp) + ((int64 emptyRowsBetween) * exp)

let galaxies = [ for x in 0..(Array2D.length1 space - 1) do 
                     for y in 0..(Array2D.length2 space - 1) do
                        yield x, y]
               |> List.filter (fun (x, y) -> space.[x, y] = '#')

let rec allPairs l a =
    match l with
    | [] -> a
    | h :: t -> allPairs t (List.append a (t |> List.map (fun x -> (h, x))))

let result1 = allPairs galaxies []
              |> List.sumBy (fun (g1, g2) -> dist emptyColumns emptyRows 1L g1 g2)

let result2 = allPairs galaxies []
              |> List.sumBy (fun (g1, g2) -> dist emptyColumns emptyRows 999999L g1 g2)
