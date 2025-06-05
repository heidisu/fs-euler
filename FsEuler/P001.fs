module FsEuler.P001

open FsEuler.Types

let sum3and5 limit =
    [ 3 .. limit - 1 ] |> List.filter (fun n -> n % 3 = 0 || n % 5 = 0) |> List.sum

let solution =
    { number = 1
      value = Int <| sum3and5 1000 }
