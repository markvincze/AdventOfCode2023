open System
open System.IO

type ModuleInfo =
    | Broadcaster
    | FlipFlop of bool
    | Conjuction of Map<string, bool>

type Module = {
    moduleInfo : ModuleInfo
    name : string
    destinations : string list
}

let parseLine (line: string) =
    let [| moduleName; destinations |] = line.Split " -> "
    let destinations = destinations.Split ", " |> List.ofArray
    match moduleName[0] with
    | '%' -> { moduleInfo = FlipFlop false
               name = moduleName.Substring(1)
               destinations = destinations }
    | '&' -> { moduleInfo = Conjuction Map.empty<string, bool>
               name = moduleName.Substring(1)
               destinations = destinations }
    | _ -> { moduleInfo = Broadcaster
             name = "broadcaster"
             destinations = destinations }

let initCojunctionInputStates (modules: Map<string, Module>) =
    let newModules = modules
                     |> Map.map (fun name m -> name, m.destinations)
                     |> Map.values
                     |> Seq.collect (fun (name, destinations) -> destinations |> List.map (fun d -> name, d))
                     |> List.ofSeq
                     |> List.filter
                        (fun (_, dest) -> match Map.tryFind dest modules with
                                          | Some { moduleInfo = Conjuction _ } -> true 
                                          | _ -> false)
                     |> List.fold
                       (fun (modules: Map<string, Module>) (src, dest) ->
                           let states = Map.add
                                            src
                                            false
                                            (match modules[dest].moduleInfo with
                                            | Conjuction states -> states)
                           Map.add
                               dest
                               { modules[dest] with moduleInfo = Conjuction states }
                               modules)
                       modules
    newModules

let modules = File.ReadAllLines "FSharp/20-pulse-propagation-input.txt"
              |> Array.map parseLine
              |> Array.map (fun m -> m.name, m)
              |> Map.ofArray
              |> initCojunctionInputStates

let rec processPulses modules pulses accLow accHigh =
    match pulses with
    | [] -> modules, accLow, accHigh
    | (pulse, source, target) :: t ->
        match Map.tryFind target modules with
        | None -> processPulses modules t (if pulse then accLow else accLow + 1) (if pulse then accHigh + 1 else accHigh)
        | Some m -> let newPulses, newModule =
                        match m.moduleInfo, pulse with
                        | Broadcaster, _ -> m.destinations |> List.map (fun d -> pulse, m.name, d), m
                        | FlipFlop _, true -> [], m
                        | FlipFlop state, false ->  m.destinations |> List.map (fun d -> not state, m.name, d),
                                                    { m with moduleInfo = FlipFlop (not state) }
                        | Conjuction inputStates, _ -> let inputStates = Map.add source pulse inputStates
                                                       let newPulse = inputStates |> Seq.forall (fun kvp -> kvp.Value) |> not
                                                       m.destinations |> List.map (fun d -> newPulse, m.name, d),
                                                       { m with moduleInfo = Conjuction inputStates }

                    processPulses (Map.add target newModule modules) (List.append t newPulses) (if pulse then accLow else accLow + 1) (if pulse then accHigh + 1 else accHigh)

let rec processNPressses modules n accLow accHigh =
    if n = 0
    then modules, accLow, accHigh
    else let modules, accLow, accHigh = processPulses modules [(false, "", "broadcaster")] accLow accHigh
         processNPressses modules (n - 1) accLow accHigh

let finalModules, low, high = processNPressses modules 1000 0 0
let result1 = (int64 low) * (int64 high)