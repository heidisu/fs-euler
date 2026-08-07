module FsEuler.P010

open Types

let isPrime (n: int64) =
    let rec check i =
        i > n / 2L || (n % i <> 0 && check (i + 1L))

    check 2L

let isPrime2 n =
    if n = 1L then
        false
    elif n = 2L then
        true
    else
        seq { 2L .. (n / 2L + 1L) }
        |> Seq.tryFind (fun i -> n % i = 0L)
        |> function
            | Some _ -> false
            | None -> true

let primeSum n =
    seq {
        for m in 2L .. n do
            if isPrime m then
                m
    }
    |> Seq.sum

let primeSum2 n =
    let rec calcSum i acc =
        if i > n then
            acc
        else
            match isPrime i with
            | true -> calcSum (i + 1L) (acc + i)
            | false -> calcSum (i + 1L) acc

    calcSum 2L 0L


let solution =
    { number = 10
      value = Int64 <| primeSum2 2000000L } // 142913828922
