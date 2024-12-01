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

let result = readInput "input.txt" |> sorted |> difference
printfn "%i" result
