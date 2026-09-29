module FsEuler.P045

open Types

let triangle n = n * (n + 1L) / 2L
let pentagonal n = n * (3L * n - 1L) / 2L
let hexagonal n = n * (2L * n - 1L)

let rec findCommonNumber i j k v1 v2 v3 =
    if v1 = v2 && v2 = v3 then
        v1
    else if v1 <= v2 && v1 <= v3 then
        findCommonNumber (i + 1L) j k (triangle (i + 1L)) v2 v3
    else if v2 <= v1 && v2 <= v3 then
        findCommonNumber i (j + 1L) k v1 (pentagonal (j + 1L)) v3
    else
        findCommonNumber i j (k + 1L) v1 v2 (hexagonal (k + 1L))

let solve i j k =
    findCommonNumber i j k (triangle i) (pentagonal j) (hexagonal k)

let solution =
    { number = 45
      value = Int64 <| solve 286L 166L 144L }
