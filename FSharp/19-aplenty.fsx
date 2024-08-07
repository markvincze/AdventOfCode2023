open System
open System.IO
open System.Text.RegularExpressions

type Part = {
    x: int
    m: int
    a: int
    s: int
}

type Comparison = Smaller | Greater

type Decision =
    | Workflow of string
    | Accepted
    | Rejected

type Condition = {
    property: char
    comparison: Comparison
    value: int
}

type Rule = {
    condition: Condition
    decision: Decision
}

type Workflow = {
    name: string
    rules: Rule list
    fallback: Decision
}

let parseDecision = function
                    | "A" -> Accepted
                    | "R" -> Rejected
                    | wf -> Workflow (wf)

let parseRule (rule : string) =
    let m = Regex.Match(rule, @"(\w+)([<>])(\d+):(\w+)")
    {
        condition = {
            property = m.Groups[1].Value[0]
            comparison = if m.Groups[2].Value = "<" then Smaller else Greater
            value = m.Groups[3].Value |> Int32.Parse
        }
        decision = parseDecision m.Groups[4].Value
    }

let parseWorkflow (line : string) =
    let m = Regex.Match(line, @"(\w+)\{(.*)\}")
    let rules = m.Groups[2].Value.Split(',')
    {
        name = m.Groups[1].Value
        rules = rules |> Array.take (Array.length rules - 1) |> Array.map parseRule |> List.ofArray
        fallback = parseDecision (Array.last rules)
    }

let parsePart (line : string) =
    let m = Regex.Match(line, @"\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}")
    {
        x = m.Groups[1].Value |> Int32.Parse
        m = m.Groups[2].Value |> Int32.Parse
        a = m.Groups[3].Value |> Int32.Parse
        s = m.Groups[4].Value |> Int32.Parse
    }

let parseInput lines =
    let (workflows, parts) = List.splitAt (List.findIndex (fun l -> l = "") lines) lines
    workflows |> List.map parseWorkflow |> List.map (fun wf -> wf.name, wf) |> Map.ofList, parts |> List.skip 1 |> List.map parsePart

let workflows, parts = File.ReadAllLines "FSharp/19-aplenty-input.txt"
                       |> List.ofArray
                       |> parseInput

let runWorkflow workflow part =
    let rec runWorkflow rules fallback part =
        match rules with
        | [] -> fallback
        | h :: t -> let propValue = match h.condition.property with
                                    | 'x' -> part.x
                                    | 'm' -> part.m
                                    | 'a' -> part.a
                                    | 's' -> part.s
                    let isSatisfied = match h.condition.comparison with
                                      | Smaller -> propValue < h.condition.value
                                      | Greater -> propValue > h.condition.value
                    if isSatisfied
                    then h.decision
                    else runWorkflow t fallback part

    runWorkflow workflow.rules workflow.fallback part

let evaluate workflows part =
    let rec evaluate workflows part workflowName =
        let workflow = Map.find workflowName workflows
        match runWorkflow workflow part with
        | Workflow wf -> evaluate workflows part wf
        | decision -> decision
    
    evaluate workflows part "in"

let result1 = parts
              |> List.map (fun p -> p, evaluate workflows p)
              |> List.filter (fun (_, d) -> d = Accepted)
              |> List.sumBy (fun (p, _) -> p.x + p.m + p.a + p.s)

// Part 2
type Path = {
    conditions: Condition list
    decision: Decision
}

let invert = function
             | { property = property; value = value; comparison = Smaller } -> { property = property; value = value - 1; comparison = Greater }
             | { property = property; value = value; comparison = Greater } -> { property = property; value = value + 1; comparison = Smaller }

let rec collectAllPaths workflows workflowName conditions =
    let workflow = Map.find workflowName workflows

    let rec collectPathsForWorkflow (rules : Rule list) fallback conditions acc =
        match rules with
        | [] -> List.append
                    acc
                    (match fallback with
                     | Workflow wf -> collectAllPaths workflows wf conditions
                     | decision -> [{ conditions =  conditions; decision = decision }])
        | h :: t -> let acc = List.append
                                  acc
                                  (match h.decision with
                                   | Workflow wf -> collectAllPaths workflows wf (h.condition :: conditions)
                                   | decision -> [{ conditions = h.condition :: conditions; decision = decision }])
                    collectPathsForWorkflow t fallback ((invert h.condition) :: conditions) acc

    collectPathsForWorkflow workflow.rules workflow.fallback conditions []

let paths = collectAllPaths workflows "in" []

let legalCombinations conditions =
    let rec legalRange conditions minValue maxValue =
        match conditions with
        | [] -> max (maxValue - minValue + 1L) 0L
        | h :: t -> match h with 
                    | { property = _; value = value; comparison = Smaller } -> legalRange t minValue (min maxValue ((int64 value) - 1L))
                    | { property = _; value = value; comparison = Greater } -> legalRange t (max minValue ((int64 value) + 1L)) maxValue 
    
    let xRange = legalRange (conditions |> List.filter (fun c -> c.property = 'x')) 1 4000
    let mRange = legalRange (conditions |> List.filter (fun c -> c.property = 'm')) 1 4000
    let aRange = legalRange (conditions |> List.filter (fun c -> c.property = 'a')) 1 4000
    let sRange = legalRange (conditions |> List.filter (fun c -> c.property = 's')) 1 4000

    xRange * mRange * aRange * sRange

let result2 = paths
              |> List.filter (fun p -> p.decision = Accepted)
              |> List.sumBy (fun p -> legalCombinations p.conditions)
