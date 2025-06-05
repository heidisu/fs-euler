module FsEuler.P002

open FsEuler.Types

let fib n =
    (1, 2)
    |> Seq.unfold (fun (curr, next) -> if curr > n then None else Some(curr, (next, curr + next)))

let value = fib 4000000 |> Seq.filter (fun n -> n % 2 = 0) |> Seq.sum

let solution = { number = 2; value = Int <| value }
