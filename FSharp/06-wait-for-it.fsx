let getMinMax time distance =
    ((time - sqrt((pown time 2) - 4.0 * distance)) / 2.0),
    ((time + sqrt((pown time 2) - 4.0 * distance)) / 2.0)

let getNumberOfWaysToWin time distance =
    let (minWait, maxWait) = getMinMax time distance
    let minWaitAdjusted = if minWait = round(minWait)
                          then minWait + 1.0 |> int64
                          else ceil(minWait) |> int64
    let maxWaitAdjusted = if maxWait = round(maxWait)
                          then maxWait - 1.0 |> int64
                          else floor(maxWait) |> int64
    
    maxWaitAdjusted - minWaitAdjusted + 1L

// let result1 = [ (7, 9); (15, 40); (30, 200) ]
let result1 = [ (44, 202); (82, 1076); (69, 1138); (81, 1458) ]
              |> List.map (fun (t, d) -> getNumberOfWaysToWin t d)
              |> List.reduce (*)

// let result2 = [ (71530, 940200) ]
let result2 = [ (44826981L, 202107611381458L) ]
              |> List.map (fun (t, d) -> getNumberOfWaysToWin (float t) (float d))
              |> List.reduce (*)
