open System
open System.IO

let rec maximum (arr: float list) =
    match arr with
    | [x] -> x
    | x::xs -> max x (maximum xs)
    | [] -> -1.

let rec minimum (arr: float list) = 
    match arr with
    | [x] -> x
    | x::xs -> min x (minimum xs)
    | [] -> -1.

let rec sum (arr: float list) =
    match arr with
    | x::xs -> x + sum xs
    | [] -> 0.

let avg (arr: float list) = 
    sum arr / float arr.Length

let sd (arr: float list) =
    let avg = avg arr
    let n_arr = [for i in arr do yield (i - avg) ** 2.]
    (sum n_arr / float arr.Length) ** 0.5

let numsInRange (arr: float list, min, max) =
    let rec count (arr: float list, min, max, cnt) = 
        match arr with
        | [] -> 0.
        | x::xs -> cnt + count(xs, min, max, cnt) + if (x < min) || (x > max) then 0. else 1.

    count(arr, min, max, 0.) / float arr.Length * 100.

let getSenteces (fileName: string) =
    let text = String.Concat(File.ReadLines(fileName))
    text.Split([| '.' |], StringSplitOptions.RemoveEmptyEntries)

let numOfWords (senteces: string array) =
    let rec count (senteces: string list, cnt) =
        match senteces with
        | [] -> 0
        | x::xs -> cnt + x.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries).Length + count(xs, cnt)

    count(senteces |> Array.toList, 0)

let minWordLen (senteces: string array) =
    let n_arr = [| for sentec in senteces do for word in sentec.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) do yield word.Length |]

    if n_arr.Length > 0 then
        Array.min n_arr
    else 0

let maxWordLen (senteces: string array) =
    let n_arr = [| for sentec in senteces do for word in sentec.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) do yield word.Length |]
    
    if n_arr.Length > 0 then
        Array.max n_arr
    else 0

let avgWordLen (senteces: string array) =
    let n_arr = [| for sentec in senteces do for word in sentec.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) do yield (float word.Length) |]

    if n_arr.Length > 0 then
        Array.average n_arr
    else 0.

let minWordsInSentec (senteces: string array) =
    let n_arr = [for sentec in senteces do yield (float (sentec.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries).Length))]
    List.min n_arr

let maxWordsInSentec (senteces: string array) =
    let n_arr = [for sentec in senteces do yield (float (sentec.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries).Length))]
    List.max n_arr

let avgWordsInSentec (senteces: string array) =
    let n_arr = [for sentec in senteces do yield (float (sentec.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries).Length))]
    List.average n_arr

let rand = new System.Random()
let arr = [for i in 1..100 do yield rand.NextDouble()]
printfn "arr: %A" arr

printfn "max: %f" (maximum arr)

printfn "min: %f" (minimum arr)

printfn "avg: %f" (avg arr)
printfn "sd: %f" (sd arr)
printfn "nums in range [0.0, 0.25]: %f" (numsInRange(arr, 0.0, 0.25))
printfn "nums in range [0.25, 0.5]: %f" (numsInRange(arr, 0.25, 0.5))
printfn "nums in range [0.5, 0.75]: %f" (numsInRange(arr, 0.5, 0.75))
printfn "nums in range [0.75, 1.0]: %f" (numsInRange(arr, 0.75, 1.0))


let senteces = getSenteces("input.txt")
printfn "number of words in text: %d" (numOfWords senteces)
printfn "min word length: %d" (minWordLen senteces)
printfn "max word length: %d" (maxWordLen senteces)
printfn "avg word length: %f" (avgWordLen senteces)
printfn "min words in sentec: %f" (minWordsInSentec senteces)
printfn "max words in sentec: %f" (maxWordsInSentec senteces)
printfn "avg words in sentec: %f" (avgWordsInSentec senteces)