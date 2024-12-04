open System.IO

let readInput path =
    File.ReadLines(path)
    |> Seq.map (fun s -> s.ToCharArray() |> Array.map string)
    |> array2D

type Path =
    { Positions: (int * int) list
      Direction: (int * int) option }

let findNeighbors (input: string[,]) (x, y) (target: string) direction =
    let directions =
        match direction with
        | None -> [ (-1, 0); (1, 0); (0, 1); (0, -1); (-1, 1); (-1, -1); (1, 1); (1, -1) ]
        | Some direction -> [ direction ]

    let check (dx, dy) =
        let nx, ny = x + dx, y + dy

        let withinBounds =
            nx >= 0 && ny >= 0 && nx < input.GetLength(0) && ny < input.GetLength(1)

        if withinBounds && input.[nx, ny] = target then
            Some((nx, ny), direction)
        else
            None

    Seq.choose check directions

let positionsOf target (input: string[,]) =
    seq {
        for x in 0 .. input.GetLength(0) - 1 do
            for y in 0 .. input.GetLength(1) - 1 do
                if input.[x, y] = target then
                    yield (x, y)
    }
    |> Set.ofSeq

let rec explore input paths targets =
    match targets with
    | [] -> paths
    | target :: nextTargets ->
        let nextPaths =
            paths
            |> Seq.collect (fun path ->
                let (x, y) = List.head path.Positions
                let visited = Set.ofList path.Positions
                let currentDirection = path.Direction

                findNeighbors input (x, y) target currentDirection
                |> Seq.filter (fun ((nx, ny), _) -> not (visited.Contains(nx, ny)))
                |> Seq.map (fun ((nx, ny), direction) ->
                    { Positions = (nx, ny) :: path.Positions
                      Direction = direction }))
            |> Seq.cache

        explore input nextPaths nextTargets

let printPaths paths (input: string[,]) =
    let printPath path =
        List.rev path.Positions
        |> List.map (fun (x, y) -> sprintf "%s" input.[x, y])
        |> String.concat " -> "
        |> printfn "%s"

    paths
    |> Seq.iter (fun path ->
        path.Positions
        |> List.rev
        |> List.map (fun (x, y) -> sprintf "(%d, %d)" x y)
        |> String.concat " -> "
        |> printfn "%s"

        printPath path
        printfn "%A\n" path.Direction.Value)

let input = readInput "input.txt"
let positions = positionsOf "X" input

let initial =
    positions
    |> Seq.map (fun pos ->
        { Positions = [ pos ]
          Direction = None })

let paths = explore input initial [ "M"; "A"; "S" ]
printPaths paths input
printfn "%i" <| Seq.length paths
