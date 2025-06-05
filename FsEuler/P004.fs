module FsEuler.P004

open Types

let isPalindrome (n: int) =
    let numList = string n |> Seq.toList
    let length = List.length numList

    if length % 2 <> 0 then
        false
    else
        let firstHalf = List.take (length / 2) numList
        let secondHalf = List.skip (length / 2) numList
        firstHalf = List.rev secondHalf

let maxPalindrome numDigits =
    let startIdx = pown 10 (numDigits - 1)
    let endIdx = (pown 10 numDigits) - 1
    let digits = [ startIdx..endIdx ]

    digits
    |> List.collect (fun d -> digits |> List.map (fun i -> i * d) |> List.filter isPalindrome)
    |> List.max

let solution =
    { number = 4
      value = Int(maxPalindrome 3) }
