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

type Rule = {
    property: char
    comparison: Comparison
    value: int
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
        property = m.Groups[1].Value[0]
        comparison = if m.Groups[2].Value = "<" then Smaller else Greater
        value = m.Groups[3].Value |> Int32.Parse
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
        | h :: t -> let propValue = match h.property with
                                    | 'x' -> part.x
                                    | 'm' -> part.m
                                    | 'a' -> part.a
                                    | 's' -> part.s
                    let isSatisfied = match h.comparison with
                                      | Smaller -> propValue < h.value
                                      | Greater -> propValue > h.value
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
