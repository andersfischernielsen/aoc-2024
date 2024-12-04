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

let positionsOf (input: string[,]) target =
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
    | target :: restTargets ->
        let newPaths =
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

        if Seq.isEmpty newPaths then
            Seq.empty
        else
            explore input newPaths restTargets

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
let positions = positionsOf input "X"

let initial =
    positions
    |> Seq.map (fun pos ->
        { Positions = [ pos ]
          Direction = None })

let paths = explore input initial [ "M"; "A"; "S" ]
printPaths paths input
printfn "%i" <| Seq.length paths
