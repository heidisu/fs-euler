module FsEuler.P003

open Common
open Types

let maxFactor n = factorize n 2L [] |> List.max

let solution =
    { number = 3
      value = Int64 <| maxFactor 600851475143L }
