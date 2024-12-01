open System.Reflection
open System.IO

let readInput path =
    let map (l: string) = l.Split "   " |> List.ofArray

    let raw = File.ReadLines(path)
    let mapped = Seq.map map raw
    let numbers = Seq.map (fun elements -> List.map (fun s -> int s) elements) mapped
    numbers

let split (acc: list<int> * list<int>) (e: list<int>) =
    match e with
    | xs :: ys :: _ ->
        match acc with
        | axs, abs -> xs :: axs, ys :: abs
    | _ -> acc

let sorted (input: seq<list<int>>) =
    let separated = Seq.fold split ([], []) input

    match separated with
    | left, right -> List.sort left, List.sort right

let difference (lists: list<int> * list<int>) =
    let zipped =
        match lists with
        | left, right -> List.zip left right

    let calculate e =
        match (e: int * int) with
        | l, r -> abs (l - r)

    List.map calculate zipped |> List.sum


let occurrences (list: list<int>) =
    list
    |> Seq.groupBy id
    |> Seq.map (fun (key, group) -> key, Seq.length group)
    |> Map.ofSeq

let multiplyIfPresent (map: Map<int, int>) l =
    match map.TryFind(l) with
    | Some e -> l * e
    | None -> 0

let occurence (lists: list<int> * list<int>) =
    let mapped =
        match lists with
        | (_, right) -> occurrences right

    let calculated =
        match lists with
        | (left, _) -> multiplyIfPresent mapped |> List.map <| left

    List.sum calculated

let input = readInput "input.txt" |> sorted
let firstResult = input |> difference
let secondResult = input |> occurence
printfn "%i" firstResult
printfn "%i" secondResult
