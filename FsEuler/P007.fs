module FsEuler.P007

open Types

let rec nthPrimeSearch n acc curr =
    if n = 0 then
        acc
    else
        match List.tryFind (fun n -> curr % n = 0) acc with
        | Some _ -> nthPrimeSearch n acc (curr + 1)
        | None -> nthPrimeSearch (n - 1) (curr :: acc) (curr + 1)

let nthPrime n =
    nthPrimeSearch (n - 1) [ 2 ] 3 |> List.head

let solution =
    { number = 7
      value = Int <| nthPrime 10001 }
