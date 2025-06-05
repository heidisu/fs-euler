module FsEuler.Types

type Value =
    | Int of int
    | Int64 of int64

type Solution = { number: int; value: Value }
