open System
open System.IO

let parseLine (line: string) =
    let parts = line.Split ": "
    let numbers = parts.[1].Split " | "
    let winningNumbers = numbers[0].Split(' ', (StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries))
                         |> Array.map Int32.Parse
                         |> Set.ofArray

    let ownNumbers = numbers[1].Split(' ', (StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries))
                         |> Array.map Int32.Parse
                         |> Set.ofArray
    
    winningNumbers, ownNumbers

let calculateWinCount card = Set.intersect (fst card) (snd card)
                             |> Set.count

let calculatePoints card =
    match calculateWinCount card with
    | 0 -> 0
    | winCount -> pown 2 (winCount - 1)

let result1 = File.ReadAllLines "FSharp/04-scratchcards-input.txt"
              |> Array.map parseLine
              |> List.ofArray
              |> List.map calculatePoints
              |> List.sum

let rec collectCopies (cards: (Set<int> * Set<int>) array) (copies: int array) index =
    if index >= Array.length cards
    then copies |> Array.sum
    else let winCount = calculateWinCount (cards.[index])
         for i in index + 1..index + winCount do
             copies.[i] <- copies.[i] + (copies.[index])
         collectCopies cards copies (index + 1)

let cardsArray = File.ReadAllLines "FSharp/04-scratchcards-input.txt"
                 |> Array.map parseLine

let result2 = collectCopies cardsArray (Array.create (Array.length cardsArray) 1) 0
