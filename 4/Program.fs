open System.IO

let readInput path =
    File.ReadLines(path)
    |> Seq.map (fun s -> s.ToCharArray() |> Array.map string)
    |> array2D

type Direction =
    | North
    | South
    | East
    | West
    | Northeast
    | Northwest
    | Southeast
    | Southwest

type Path =
    { Positions: (int * int) list
      Direction: Direction option }

let toCoordinates direction =
    match direction with
    | North -> (-1, 0)
    | South -> (1, 0)
    | East -> (0, 1)
    | West -> (0, -1)
    | Northeast -> (-1, 1)
    | Northwest -> (-1, -1)
    | Southeast -> (1, 1)
    | Southwest -> (1, -1)

let findNeighbors (input: string[,]) (x, y) (target: string) (direction: Direction option) =
    let directions =
        match direction with
        | None -> [ North; South; East; West; Northeast; Northwest; Southeast; Southwest ]
        | Some direction -> [ direction ]

    let check direction =
        let dx, dy = toCoordinates direction
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
                |> Seq.map (fun ((nx, ny), dir) ->
                    { Positions = (nx, ny) :: path.Positions
                      Direction = Some dir }))
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
let xPositions = positionsOf "X" input

let initial =
    xPositions
    |> Seq.map (fun pos ->
        { Positions = [ pos ]
          Direction = None })

let paths = explore input initial [ "M"; "A"; "S" ]
printfn "%i" <| Seq.length paths

let hasCross (x, y) (input: string[,]) : bool =
    let withinBounds =
        x > 0 && y > 0 && x < input.GetLength(0) - 1 && y < input.GetLength(1) - 1

    if withinBounds then
        let mas1 = (input.[x - 1, y - 1] = "M" && input.[x + 1, y + 1] = "S")
        let mas2 = (input.[x + 1, y - 1] = "M" && input.[x - 1, y + 1] = "S")

        let sam1 = (input.[x - 1, y - 1] = "S" && input.[x + 1, y + 1] = "M")
        let sam2 = (input.[x + 1, y - 1] = "S" && input.[x - 1, y + 1] = "M")

        [ mas1; mas2; sam1; sam2 ] |> List.filter id |> List.length >= 2
    else
        false

let aPositions = positionsOf "A" input
let crosses = Seq.filter (fun e -> hasCross e input) aPositions
printfn "%i" <| Seq.length crosses
