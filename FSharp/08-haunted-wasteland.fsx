open System
open System.IO

type Element = {
    name: string
    left: string
    right: string
}

let parseElement (line: string) =
    let [| name; rest |] = line.Split(" = (")
    let [| left; right |] = rest.Split(", ")

    name, (left, right.Substring(0, right.IndexOf ')'))
    // {
    //     name = name
    //     left = left
    //     right = right.Substring(0, right.IndexOf ')')
    // }

let parse lines =
    let steps = List.head lines
    let elements = lines |> List.skip 2 |> List.map parseElement |> Map.ofList
    steps, elements

let steps, elements = File.ReadAllLines "FSharp/08-haunted-wasteland-input.txt" |> List.ofArray |> parse

let step s pos elements =
    match s with
    | 'L' -> Map.find pos elements |> fst
    | 'R' -> Map.find pos elements |> snd
    | _ -> failwith "Invalid step input"

let rec stepUntilEnd (steps: string) elements pos count =
    if pos = "ZZZ"
    then count
    else let s = steps.[count % steps.Length]
         stepUntilEnd steps elements (step s pos elements) (count + 1)

let result1 = stepUntilEnd steps elements "AAA" 0