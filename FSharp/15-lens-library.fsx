open System
open System.IO

let instructions =
    (File.ReadAllText "FSharp/15-lens-library-input.txt").Split ','

let rec hash =
    Seq.fold (fun current c -> (((current + (int c)) * 17) % 256)) 0

let result1 = instructions
              |> Array.sumBy hash

// Part 2
type Lens = {
    label: string
    length: int
}

type Box = Lens list

type Step =
| Remove of int * string
| Add of int * string * int

let parseStep (str: string) = if str.Contains '-'
                              then let label = str.TrimEnd '-'
                                   Remove (hash label, label)
                              else let [| label; length |] = str.Split '='
                                   Add (hash label, label, length |> Int32.Parse)

let steps = instructions |> Array.map parseStep |> List.ofArray

let boxes = Array.create 256 []

steps
|> List.iter (fun step ->
                 match step with
                 | Remove (index, label) -> match List.tryFindIndex (fun l -> l.label = label) boxes.[index] with
                                            | Some i -> boxes.[index] <- List.removeAt i boxes.[index]
                                            | None -> ()
                 | Add (index, label, length) -> match List.tryFindIndex (fun l -> l.label = label) boxes.[index] with
                                                 | Some i -> boxes.[index] <- boxes.[index] |> List.removeAt i |> List.insertAt i { label = label; length = length }
                                                 | None -> boxes.[index] <- List.append boxes.[index] [ { label = label; length = length } ])

let result2 = boxes
              |> Array.indexed
              |> Array.sumBy (fun (bi, ls) -> ls |> List.indexed |> List.sumBy (fun (li, l) -> (bi + 1) * (li + 1) * l.length))
