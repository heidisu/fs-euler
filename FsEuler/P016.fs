module FsEuler.P016

open Types

let rec sumOfDigits acc number =
    if number < 10I then
        acc + number
    else
        sumOfDigits (acc + number % 10I) (number / 10I)

let solution =
    { number = 16
      value = BigInt <| sumOfDigits 0I (pown 2I 1000) }
