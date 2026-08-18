module FsEuler.Program

open FsEuler
open FsEuler.Types

let getValue value =
    match value with
    | Int i -> string i
    | Int64 i -> string i
    | BigInt i -> string i

[<EntryPoint>]
let main _ =
    seq {
        P001.solution
        P002.solution
        P003.solution
        P004.solution
        P005.solution
        P006.solution
        P007.solution
        P008.solution
        P009.solution
        //P010.solution Very slow
        P011.solution
        P013.solution
        P014.solution
        P015.solution
        P016.solution
        P017.solution
        P018.solution
    }
    |> Seq.iter (fun solution -> printfn $"Problem {solution.number}: {getValue solution.value}")

    0
