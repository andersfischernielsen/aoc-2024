open System.IO
open System.Text.RegularExpressions

let readInput path =
    let raw = File.ReadLines(path)
    List.ofSeq raw |> String.concat ""

let map (m: Match) =
    let x, y = int m.Groups.[1].Value, int m.Groups.[2].Value
    (x, y)

let extractMul (input: string) =
    let regex = Regex(@"mul\((\d+),(\d+)\)")
    regex.Matches(input) |> Seq.map map

let extractSections (input: string) =
    let regex = Regex(@"^(.*?don't\(\))|do\(\)(.*?)(?:don't\(\)|$)")

    regex.Matches(input)
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> List.ofSeq

let multiply sequence =
    sequence |> Seq.fold (fun acc (x, y) -> acc + x * y) 0

let input = readInput "input.txt"
let sections = extractSections input
let sectionResults = Seq.map (fun section -> section |> extractMul) sections
let multiplied = Seq.map multiply sectionResults
let result = multiplied |> Seq.fold (+) 0
