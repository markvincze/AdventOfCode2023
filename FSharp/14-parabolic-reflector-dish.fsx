open System
open System.IO

let lines = File.ReadAllLines "FSharp/14-parabolic-reflector-dish-input.txt"

let dish1 = Array2D.init 
               lines.[0].Length
               lines.Length
               (fun x y -> lines.[y].[x])

let dish2 = Array2D.copy dish1

type RollDirection = North | South | West | East

let rec rollUntilPossible (dish: char[,]) rollDirection x y =
    let reachedEdge, newX, newY = match rollDirection with
                                  | North -> y <= 0, x, y - 1
                                  | South -> y >= (Array2D.length2 dish - 1), x, y + 1
                                  | West -> x <= 0, x - 1, y
                                  | East -> x >= (Array2D.length1 dish - 1), x + 1, y

    match dish.[x, y] with
    | 'O' -> if reachedEdge || dish.[newX, newY] <> '.'
             then dish
             else dish.[newX, newY] <- 'O'
                  dish.[x, y] <- '.'
                  rollUntilPossible dish rollDirection newX newY
    | _ -> dish

let rollAllRocks (dish: char[,]) rollDirection =
    let coordsInOrder = match rollDirection with
                        | North -> 
                            [ for y in 0 .. Array2D.length2 dish - 1 do
                                for x in 0 .. Array2D.length1 dish - 1 do
                                    yield (x, y) ]
                        | South ->
                            [ for y in (Array2D.length2 dish - 1) .. -1 .. 0 do
                                for x in 0 .. Array2D.length1 dish - 1 do
                                    yield (x, y) ]
                        | West ->
                            [ for x in 0 .. Array2D.length1 dish - 1 do
                                for y in 0 .. Array2D.length2 dish - 1 do
                                    yield (x, y) ]
                        | East ->
                            [ for x in (Array2D.length1 dish - 1) .. -1 .. 0 do
                                for y in 0 .. Array2D.length2 dish - 1 do
                                    yield (x, y) ]

    coordsInOrder
    |> List.iter (fun (x, y) -> rollUntilPossible dish rollDirection x y |> ignore)

let totalWeight dish = 
    [ for y in 0..Array2D.length2 dish - 1 do
        for x in 0..Array2D.length1 dish - 1 do
            yield (x, y) ]
    |> List.sumBy (fun (x, y) -> match dish.[x, y] with
                                 | 'O' -> (Array2D.length2 dish) - y
                                 | _ -> 0)

rollAllRocks dish1 North

let result1 = totalWeight dish1

// Part2: this does not print the actual solution, but prints messages based on which the cycle can be determined by eye, and the result calculated with a modulo operation.
let roll4Dir cycle (dish: char[,]) =
    let copy = Array2D.copy dish
    rollAllRocks dish North
    rollAllRocks dish West
    rollAllRocks dish South
    rollAllRocks dish East
    printfn "Total weight after cycle #%d: %d" cycle (totalWeight dish)

for i in 0..1000000000 do
    if i % 100000 = 0 then printfn "Processing cycle %d" i
    roll4Dir (i + 1) dish2

let result2 = totalWeight dish2
