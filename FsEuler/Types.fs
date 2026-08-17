module FsEuler.Types

type Value =
    | Int of int
    | Int64 of int64
    | BigInt of bigint

type Solution = { number: int; value: Value }
