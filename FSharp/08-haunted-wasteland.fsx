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

let stepAll s (ps: string array) elements =
    for i in 0..(Array.length ps - 1) do
        ps.[i] <- step s ps.[i] elements

let rec stepUntilEnd3 (steps: string) elements (ps: string array) count =
    if ps |> Array.forall (fun p -> p.EndsWith("Z"))
    then count
    else let s = steps.[count % steps.Length]
         stepAll s ps elements
         stepUntilEnd3 steps elements ps (count + 1)

// Brute force approach, too slow.
// let result3 = stepUntilEnd3 steps elements (elements |> Map.keys |> Seq.filter (fun p -> p.EndsWith("A")) |> Array.ofSeq) 0

let rec findCycle (steps: string) elements (pos: string) count firstHit =
    match pos.EndsWith "Z", firstHit with
    | true, None ->
        let s = steps.[count % steps.Length]
        findCycle steps elements (step s pos elements) (count + 1) (Some (pos, count))
    | true, Some (p, c) -> (p, c), (pos, count)
    | false, _ ->
        let s = steps.[count % steps.Length]
        findCycle steps elements (step s pos elements) (count + 1) firstHit

let cycles =
    elements
    |> Map.keys 
    |> Seq.filter (fun p -> p.EndsWith("A"))
    |> List.ofSeq
    |> List.map (fun p -> p, (findCycle steps elements p 0 None))

let cycleNumbers = cycles |> List.map (snd >> fst >> snd) |> List.map int64

// Least common multiple implementation
let rec gcd a b =
    if b = 0L then a
    else gcd b (a % b)

let lcm a b =
    if a = 0L && b = 0L then 0L
    else abs (a * b) / gcd a b

let lcmOfList numbers =
    List.fold lcm 1L numbers

let result2 = lcmOfList cycleNumbers
