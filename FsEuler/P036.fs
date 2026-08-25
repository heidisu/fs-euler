module FsEuler.P036

open System
open Types

let reverse (input: string) =
    input |> Seq.rev |> System.String.Concat

let isDoublePalidrome (n: int) =
    let binary = Convert.ToString(n, 2)
    let nString = string n
    binary = reverse binary && nString = reverse nString

let doublePalidromeSum max =
    seq { 1 .. (max - 1) } |> Seq.filter isDoublePalidrome |> Seq.sum

let solution =
    { number = 36
      value = Int <| doublePalidromeSum 1000000 }
