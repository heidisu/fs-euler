module FsEuler.P028

open Types

let calculateDiagonalSum rounds =
    seq { 1 .. rounds - 1 }
    |> Seq.fold
        (fun (sum, num) i ->
            let diagonalSum =
                seq { num + 8 * i - 1 .. -1 .. num }
                |> Seq.chunkBySize (2 * i)
                |> Seq.map Array.head
                |> Seq.sum

            (sum + diagonalSum, num + 8 * i))
        (1, 2)
    |> fst

let numberSpiralDiagonal size =
    if size = 1 then
        1
    else
        let rounds = (size + 1) / 2
        calculateDiagonalSum rounds

let solution =
    { number = 28
      value = Int <| numberSpiralDiagonal 1001 }
