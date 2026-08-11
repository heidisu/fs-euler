module FsEuler.P015

open Types

let nextPossibleSteps size (x, y) =
    [ (x + 1, y); (x, y + 1) ] |> List.filter (fun (x, y) -> x <= size && y <= size)

let rec naiveSearch size steps paths =
    if steps = 0 then
        paths
    else
        let newPaths = paths |> List.collect (fun (x, y) -> nextPossibleSteps size (x, y))
        naiveSearch size (steps - 1) newPaths

let rec betterSearch size steps paths =
    if steps = 0 then
        paths |> List.head |> snd
    else
        let newPaths =
            paths
            |> List.collect (fun ((x, y), c) -> nextPossibleSteps size (x, y) |> List.map (fun p -> (p, c)))
            |> List.groupBy (fun (p, c) -> p)
            |> List.map (fun (p, l) -> (p, List.sumBy snd l))

        betterSearch size (steps - 1) newPaths

let solution =
    { number = 15
      value = Int64 <| betterSearch 20 40 [ ((0, 0), 1L) ] }
