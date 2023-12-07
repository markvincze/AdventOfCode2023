open System
open System.IO

type HandType =
    | FiveOfAKind of int
    | FourOfAKind of int
    | FullHouse of int * int
    | ThreeOfAKind of int
    | TwoPair of int * int
    | OnePair of int
    | HighCard of int

type Hand = int array

let getHandTypeScore = function
                       | FiveOfAKind _ -> 6
                       | FourOfAKind _ -> 5
                       | FullHouse _ -> 4
                       | ThreeOfAKind _ -> 3
                       | TwoPair _ -> 2
                       | OnePair _ -> 1
                       | HighCard _ -> 0

let getHandType (hand: Hand) =
    let groups = hand |> Array.groupBy id |> Array.map snd |> Array.sortByDescending (fun g -> (g |> Array.length), g.[0])

    if groups |> Array.length = 1 then FiveOfAKind (groups.[0].[0]), []
    else if groups |> Array.length = 2
    then if groups.[0] |> Array.length = 4
         then FourOfAKind (groups.[0].[0]), [ groups.[1][0] ]
         else FullHouse (if groups.[0].[0] > groups.[1].[0] then (groups.[0].[0], groups.[1][0]) else (groups.[1].[0], groups.[0][0])), []
    else if groups |> Array.length = 3
    then if groups.[0] |> Array.length = 3
         then ThreeOfAKind (groups.[0].[0]), [ groups.[1].[0]; groups.[2].[0] ]
         else TwoPair (if groups.[0].[0] > groups.[1].[0] then (groups.[0].[0], groups.[1][0]) else (groups.[1].[0], groups.[0][0])), [ groups.[2].[0] ]
    else if groups |> Array.length = 4
    then OnePair groups.[0].[0], [ groups.[1].[0]; groups.[2].[0]; groups.[3].[0] ]
    else HighCard groups.[0].[0], [ groups.[1].[0]; groups.[2].[0]; groups.[3].[0]; groups.[4].[0] ]

let comparePairwise cs1 cs2 =
    match Seq.zip cs1 cs2 |> Seq.tryFind (fun (x, y) -> x <> y) with
    | Some (x, y) -> compare x y
    | None -> failwith "There should be no draws"

let compareHands h1 h2 =
    let ht1, _ = getHandType h1
    let ht2, _ = getHandType h2

    if getHandTypeScore ht1 < getHandTypeScore ht2 then -1
    else if getHandTypeScore ht1 > getHandTypeScore ht2 then 1
    else comparePairwise h1 h2
        
        // Oops
        //  | FiveOfAKind x1, FiveOfAKind x2 -> compare x1 x2
        //  | FourOfAKind x1, FourOfAKind x2 when x1 <> x2 -> compare x1 x2
        //  | FourOfAKind _, FourOfAKind _ -> compareRemaining cs1 cs2
        //  | FullHouse (x1, _), FullHouse (x2, _) when x1 <> x2 -> compare x1 x2
        //  | FullHouse (_, y1), FullHouse (_, y2) when y1 <> y2 -> compare y1 y2
        //  | FullHouse _, FullHouse _ -> compareRemaining cs1 cs2
        //  | ThreeOfAKind x1, ThreeOfAKind x2 when x1 <> x2 -> compare x1 x2
        //  | ThreeOfAKind _, ThreeOfAKind _ -> compareRemaining cs1 cs2
        //  | TwoPair (x1, _), TwoPair (x2, _) when x1 <> x2 -> compare x1 x2
        //  | TwoPair (_, y1), TwoPair (_, y2) when y1 <> y2 -> compare y1 y2
        //  | TwoPair _, TwoPair _ -> compareRemaining cs1 cs2
        //  | OnePair x1, OnePair x2 when x1 <> x2 -> compare x1 x2
        //  | OnePair _, OnePair _ -> compareRemaining cs1 cs2
        //  | HighCard x1, HighCard x2 when x1 <> x2 -> compare x1 x2
        //  | HighCard _, HighCard _ -> compareRemaining cs1 cs2
        //  | _ -> failwith "Some case was not handled."

let parseLine (line: string) =
    let [| hand; bid |] = line.Split ' '
    hand |> Seq.map (fun c -> match c with
                              | 'T' -> 10
                              | 'J' -> 11
                              | 'Q' -> 12
                              | 'K' -> 13
                              | 'A' -> 14
                              | c -> (int c) - 48)
         |> Array.ofSeq, (Int64.Parse bid)

let hands = File.ReadAllLines "FSharp/07-camel-cards-input.txt"
            |> Array.map parseLine
            |> List.ofArray
            // |> List.sortWith compareHands
            // |> List.map getHandType

let result1 = hands
              |> List.sortWith (fun (h1, _) (h2, _) -> compareHands h1 h2)
              |> List.map snd
              |> List.indexed
              |> List.sumBy (fun (rank, bid) -> (rank + 1 |> int64) * bid)
