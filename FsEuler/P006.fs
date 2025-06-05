module FsEuler.P006

open Types

let squareSumDifference digit = 
    let digits = [1 .. digit]
    let sumOfSquares = 
        digits 
        |> List.map (fun x -> x * x)
        |> List.sum
    let squareSum = 
        digits 
        |> List.sum
        |> fun x -> x * x
    squareSum - sumOfSquares

let squareSumDifference2 digit = 
    let digits = [1 .. digit]
    digits
    |> List.collect (
        fun i -> 
            digits 
            |> List.takeWhile (fun d -> d < i)
            |> List.map (fun d -> 2 * d * i)
    )
    |> List.sum    


let solution = {
    number = 6
    value = Int <| squareSumDifference2 100
}
