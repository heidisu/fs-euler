module FsEuler.P005

open Common
open Types

let smallestDivisibleByAllNumbers n =
    let digits = [2L .. n]
    let factors = digits |> List.map (fun d -> factorize d 2L [])

    factors
    |> List.collect (
        fun l -> 
            l 
            |> List.groupBy id 
            |> List.map (fun (n, l) -> (n, List.length l))
        )
        |> List.fold (
            fun m (p, n) -> 
                m |> Map.change p (
                    fun op -> 
                        match op with
                        | Some v -> if v < n then Some n else Some v
                        | None -> Some n
                )
                
            ) Map.empty
        |> Map.toList
        |> List.fold (fun tot (p, n) -> tot * pown p n ) 1L

let solution = {
    number = 5
    value = Int64 <| smallestDivisibleByAllNumbers 20L
} 