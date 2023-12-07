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
         else FullHouse (groups.[0].[0], groups.[1][0]), []
    else if groups |> Array.length = 3
    then if groups.[0] |> Array.length = 3
         then ThreeOfAKind (groups.[0].[0]), [ groups.[1].[0]; groups.[2].[0] ]
         else TwoPair (groups.[0].[0], groups.[1][0]), [ groups.[2].[0] ]
    else if groups |> Array.length = 4
    then OnePair groups.[0].[0], [ groups.[1].[0]; groups.[2].[0]; groups.[3].[0] ]
    else HighCard groups.[0].[0], [ groups.[1].[0]; groups.[2].[0]; groups.[3].[0]; groups.[4].[0] ]

let compare h1 h2 =
    let ht1, cs1 = getHandType h1
    let ht2, cs2 = getHandType h2

    if getHandTypeScore ht1 < getHandTypeScore ht2 then -1
    else if getHandTypeScore ht1 > getHandTypeScore ht2 then -1
    else match ht1, ht2 with
         | FiveOfAKind x1, FiveOfAKind x2 -> compare x1 x2
         | FourOfAKind x1, FourOfAKind x2 when x1 <> x2 -> compare x1 x2
         | FourOfAKind x1, FourOfAKind x2 -> compare x1 x2


