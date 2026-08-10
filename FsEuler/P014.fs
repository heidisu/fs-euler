module FsEuler.P014

open System.Collections.Generic
open Types

let numToLength = Dictionary<int64, int64>()

let rec collatzSequenceLength (n: int64) (acc: int64) (curr: int64) =
    if curr = 1L then
        let result = acc + 1L
        numToLength.TryAdd(n, result) |> ignore
        result
    else
        match numToLength.TryGetValue(curr) with
        | true, l -> l + acc
        | false, _ ->
            if curr % 2L = 0L then
                collatzSequenceLength n (acc + 1L) (curr / 2L)
            else
                collatzSequenceLength n (acc + 1L) (3L * curr + 1L)

let maxSequenceLength limit =
    seq { 1L .. limit }
    |> Seq.map (fun i -> (i, collatzSequenceLength i 0 i))
    |> Seq.maxBy (fun (i, t) -> t)
    |> fst

let solution =
    { number = 14
      value = Int64 <| maxSequenceLength 1000000L }
