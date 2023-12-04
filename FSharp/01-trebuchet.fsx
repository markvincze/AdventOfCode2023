open System
open System.IO


let calibrationNumber (line : string) =
    (line
    |> Seq.filter Char.IsDigit
    |> Seq.head
    |> string
    |> Int32.Parse) * 10 +
    (line
    |> Seq.rev
    |> Seq.filter Char.IsDigit
    |> Seq.head
    |> string
    |> Int32.Parse)

let result1 = File.ReadAllLines "FSharp/01-trebuchet-input.txt"
              |> Array.sumBy calibrationNumber

let getFirstOrLastDigit (line : string) stringIndexFinder indexListSorter seqIndexFinder indexComparer =
    let numbers = [("one", 1); ("two", 2); ("three", 3); ("four", 4); ("five", 5); ("six", 6); ("seven", 7); ("eight", 8); ("nine", 9) ]

    let snum =
        numbers
        |> List.map (fun (s, n) -> (n, (stringIndexFinder line s)))
        |> List.filter (fun (_, i) -> i <> -1)
        |> indexListSorter snd
        |> List.tryHead

    let dnumIndex =
        line
        |> seqIndexFinder Char.IsDigit

    match snum, dnumIndex with
    | Some (i, _), None -> i
    | None, Some di -> line.[di] |> string |> Int32.Parse
    | Some (sn, si), Some di -> if indexComparer si di
                                then sn
                                else line.[di] |> string |> Int32.Parse
    | None, None -> failwith "No digits were found"

let calibrationNumber2 (line : string) =
    let firstDigit = getFirstOrLastDigit line (fun (src : string) (s : string) -> src.IndexOf s) List.sortBy Seq.tryFindIndex (<)
    let lastDigit = getFirstOrLastDigit line (fun (src : string) (s : string) -> src.LastIndexOf s) List.sortByDescending Seq.tryFindIndexBack (>)

    firstDigit * 10 + lastDigit

let result2 = File.ReadAllLines "FSharp/01-trebuchet-input.txt"
              |> Array.sumBy calibrationNumber2
