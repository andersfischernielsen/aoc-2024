open System.IO

let readInput path =
    let map (l: string) = l.Split " " |> List.ofArray

    let raw = File.ReadLines(path)
    let mapped = Seq.map map raw
    let numbers = Seq.map (fun elements -> List.map (fun s -> int s) elements) mapped
    numbers

type Direction =
    | Unknown
    | Up
    | Down

let rec isSafe state elements =
    match state, elements with
    | None, _ -> None
    | Some(_, _), [] -> state
    | Some(Unknown, -1), current :: rest -> isSafe (Some(Unknown, current)) rest
    | Some(Unknown, previous), current :: rest ->
        match (current - previous) with
        | diff when diff > 0 && diff <= 3 -> isSafe (Some(Up, current)) rest
        | diff when diff < 0 && diff >= -3 -> isSafe (Some(Down, current)) rest
        | _ -> None
    | Some(direction, previous), current :: rest ->
        match direction, (current - previous) with
        | Up, diff when diff > 0 && diff <= 3 -> isSafe (Some(Up, current)) rest
        | Down, diff when diff < 0 && diff >= -3 -> isSafe (Some(Down, current)) rest
        | _ -> None

let printFullList lst =
    List.iter (fun item -> printfn "%A" item) lst

let input = readInput "input.txt"
let safe = Seq.map (fun l -> isSafe (Some(Unknown, -1)) l) input
let results = Seq.filter Option.isSome safe
let zipped = Seq.zip input safe

let count = Seq.length results
printfn "Conservative safety check: %i" count

let removeOne lst =
    lst |> List.mapi (fun index _ -> List.removeAt index lst)

let unsafeBeforeRemoval =
    Seq.filter (fun (_, o) -> Option.isNone o) zipped |> List.ofSeq

let permutations = List.map (fun (i, _) -> removeOne i) unsafeBeforeRemoval

let safeAfterRemoval =
    List.map (List.filter (fun l -> isSafe (Some(Unknown, -1)) l <> None)) permutations
    |> List.filter (fun l -> List.isEmpty l <> true)


let countWithRemovals = Seq.length safeAfterRemoval
printfn "Relaxed safety check: %i" countWithRemovals

printfn "Total: %i" (count + countWithRemovals)
