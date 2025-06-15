module FsEuler.P009

open Types

let specialPythagoreanTriplets n =
    let digits = seq { 1..n }

    digits
    |> Seq.collect (fun a ->
        seq { a + 1 .. n }
        |> Seq.collect (fun b -> { b + 1 .. n } |> Seq.map (fun c -> (a, b, c))))
    |> Seq.find (fun (a, b, c) -> a + b + c = n && a * a + b * b = c * c)
    |> fun (a, b, c) -> a * b * c

let solution =
    { number = 9
      value = Int <| specialPythagoreanTriplets 1000 }
