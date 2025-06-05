module FsEuler.Program

open FsEuler
open FsEuler.Types

let getValue value =
    match value with
    | Int i -> string i
    | Int64 i -> string i

[<EntryPoint>]
let main _ =
    [ P001.solution
      P002.solution
      P003.solution
      P004.solution
      P005.solution
      P006.solution
      P007.solution ]
    |> List.iter (fun solution -> printfn $"Problem {solution.number}: {getValue solution.value}")

    0
