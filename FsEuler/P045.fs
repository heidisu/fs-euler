module FsEuler.P045

open Types

let triangle n = n * (n + 1L) / 2L
let pentagonal n = n * (3L * n - 1L) / 2L
let hexagonal n = n * (2L * n - 1L)

let rec findCommonNumber i j k v1 v2 v3 =
    seq {
        if v1 = v2 && v2 = v3 then
            yield v1

            yield!
                findCommonNumber
                    (i + 1L)
                    (j + 1L)
                    (k + 1L)
                    (triangle (i + 1L))
                    (pentagonal (j + 1L))
                    (hexagonal (k + 1L))
        else if v1 <= v2 && v1 <= v3 then
            yield! findCommonNumber (i + 1L) j k (triangle (i + 1L)) v2 v3
        else if v2 <= v1 && v2 <= v3 then
            yield! findCommonNumber i (j + 1L) k v1 (pentagonal (j + 1L)) v3
        else
            yield! findCommonNumber i j (k + 1L) v1 v2 (hexagonal (k + 1L))
    }

let solve n =
    findCommonNumber 1 1 1 (triangle 1) (pentagonal 1) (hexagonal 1)
    |> Seq.skip (n - 1)
    |> Seq.head

let solution =
    { number = 45
      value = Int64 <| solve 3 }
