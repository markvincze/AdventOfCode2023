open System
open System.IO

let lines = File.ReadAllLines "FSharp/17-clumsy-crucible-input.txt"

let pattern = Array2D.init
                  lines[0].Length
                  (Array.length lines)
                  (fun x y -> lines[y][x] |> string |> Int32.Parse)

type Direction = Up | Right | Down | Left

type Pos = {
    heatLoss: int
    nextPos: int * int
    direction: Direction
    moves: int
}

let rec findSol opts bestSol bestCache =
    if List.length opts = 0
    then bestSol
    // else if List.length opts > 30
    // then 12345
    else
        printfn ""
        printfn "Items in bestCache: %d" (Seq.length bestCache)
        printfn "Items in opts: %d" (List.length opts)
        printfn "Distinct positions in opts: %d" (opts |> List.map (fun o -> (o.nextPos, o.direction, o.moves)) |> List.distinct |> List.length)

        let filteredOpts = 
            opts
            |> List.filter (fun o -> match Map.tryFind (o.nextPos, o.direction, o.moves) bestCache with
                                     | None -> true
                                    //  | Some bestHeatLoss -> o.heatLoss < bestHeatLoss)
                                     | Some bestHeatLoss -> false)
            // |> List.filter (fun o -> Map.containsKey (o.nextPos, o.direction, o.moves) bestCache)

        printfn "Items in filteredOpts: %d" (List.length filteredOpts)


        let newBestCache =
            List.fold 
                (fun bc o -> match Map.tryFind (o.nextPos, o.direction, o.moves) bc with
                             | None -> Map.add (o.nextPos, o.direction, o.moves) o.heatLoss bc
                             | Some bestHeatLoss -> if o.heatLoss < bestHeatLoss
                                                    then Map.add (o.nextPos, o.direction, o.moves) o.heatLoss bc
                                                    else bc)
                bestCache
                opts

        printfn "Items in newBestCache: %d" (Seq.length newBestCache)

        // printfn "bestCache: %A" bestCache
        // printfn "opts: %A" opts

        let newOpts =
            filteredOpts
            |> List.collect (fun opt ->
                let (x, y) = opt.nextPos
                [
                    if y > 0 && (opt.direction = Left || opt.direction = Right)
                    then yield { heatLoss = opt.heatLoss + pattern[x, y]; nextPos = (x, y - 1); direction = Up; moves = 0 }

                    if y > 0 && opt.direction = Up && opt.moves < 3
                    then yield { heatLoss = opt.heatLoss + pattern[x, y]; nextPos = (x, y - 1); direction = Up; moves = opt.moves + 1 }

                    if x < (Array2D.length1 pattern - 1) && (opt.direction = Up || opt.direction = Down)
                    then yield { heatLoss = opt.heatLoss + pattern[x, y]; nextPos = (x + 1, y); direction = Right; moves = 0 }

                    if x < (Array2D.length1 pattern - 1) && opt.direction = Right && opt.moves < 3
                    then yield { heatLoss = opt.heatLoss + pattern[x, y]; nextPos = (x + 1, y); direction = Right; moves = opt.moves + 1 }

                    if y < (Array2D.length2 pattern - 1) && (opt.direction = Left || opt.direction = Right)
                    then yield { heatLoss = opt.heatLoss + pattern[x, y]; nextPos = (x, y + 1); direction = Down; moves = 0 }

                    if y < (Array2D.length2 pattern - 1) && opt.direction = Down && opt.moves < 3
                    then yield { heatLoss = opt.heatLoss + pattern[x, y]; nextPos = (x, y + 1); direction = Down; moves = opt.moves + 1 }

                    if x > 0 && (opt.direction = Up || opt.direction = Down)
                    then yield { heatLoss = opt.heatLoss + pattern[x, y]; nextPos = (x - 1, y); direction = Left; moves = 0 }

                    if x > 0 && opt.direction = Left && opt.moves < 3
                    then yield { heatLoss = opt.heatLoss + pattern[x, y]; nextPos = (x - 1, y); direction = Left; moves = opt.moves + 1 }
                ])

        printfn "Items in newOpts: %d" (List.length newOpts)
        // printfn "bestCache: %A" bestCache

        let newSolutions = newOpts
                           |> List.filter (fun o -> o.nextPos = (Array2D.length1 pattern - 1, Array2D.length2 pattern - 1))
                           |> List.map (fun o -> o.heatLoss + pattern[Array2D.length1 pattern - 1, Array2D.length2 pattern - 1])

        // printfn "opts: %A" (opts |> List.truncate 5)
        // printfn "newOpts: %A" (newOpts |> List.truncate 5)

        let newBestSol = if List.length newSolutions = 0
                         then bestSol
                         else min bestSol (List.min newSolutions)

        findSol (newOpts |> List.filter (fun o -> o.heatLoss < newBestSol)) newBestSol newBestCache

let result1 =
    findSol
        [
            { heatLoss = 0; nextPos = (1, 0); direction = Right; moves = 0 }
            { heatLoss = 0; nextPos = (0, 1); direction = Down; moves = 0 }
        ]
        Int32.MaxValue
        Map.empty<(int * int) * Direction * int, int>
