open System
open System.IO

let parseHistory (line: string) = line.Split ' '
                                  |> Array.map Int64.Parse
                                  |> List.ofArray

let histories = File.ReadAllLines "FSharp/09-mirage-maintenance-input.txt"
                |> Array.map parseHistory
                |> List.ofArray

let getDiff history = history
                      |> List.pairwise
                      |> List.map (fun (a, b) -> b - a)

let getDiffSeq history =
    history ::
        (history
        |> List.unfold (fun h -> if h |> List.forall (fun x -> x = 0L)
                                 then None
                                 else let d = getDiff h
                                      (d, d) |> Some))

let getPrediction1 h =
    getDiffSeq h
    |> List.map List.last
    |> List.sum

let result1 = histories
              |> List.map getPrediction1
              |> List.sum
                                                              
// Part 2
let getPrediction2 h =
    getDiffSeq h
    |> List.map List.head
    |> List.rev
    |> List.reduce (fun a b -> b - a)

let result2 = histories
              |> List.map getPrediction2
              |> List.sum
