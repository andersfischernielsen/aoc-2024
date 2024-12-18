﻿open System.IO
open System.Text.RegularExpressions

let readInput path =
    File.ReadLines(path) |> String.concat ""

let extractMul (input: string) =
    let map (m: Match) =
        let x, y = int m.Groups.[1].Value, int m.Groups.[2].Value
        (x, y)

    let regex = Regex(@"mul\((\d+),(\d+)\)")
    regex.Matches(input) |> Seq.map map

let extractSections (input: string) =
    let regex = Regex(@"^(.*?don't\(\))|do\(\)(.*?)(?:don't\(\)|$)")
    regex.Matches(input) |> Seq.map (fun m -> m.Value)

let multiply = Seq.fold (fun acc (x, y) -> acc + x * y) 0

let sections = readInput "input.txt" |> extractSections
let multiplied = Seq.map (fun section -> section |> extractMul |> multiply) sections
let result = multiplied |> Seq.fold (+) 0
printfn "%i" result
