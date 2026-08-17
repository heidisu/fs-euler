module FsEuler.P017

open Types

let numberToWordMap =
    [ (1, "one")
      (2, "two")
      (3, "three")
      (4, "four")
      (5, "five")
      (6, "six")
      (7, "seven")
      (8, "eight")
      (9, "nine")
      (10, "ten")
      (11, "eleven")
      (12, "twelve")
      (13, "thirteen")
      (14, "fourteen")
      (15, "fifteen")
      (16, "sixteen")
      (17, "seventeen")
      (18, "eighteen")
      (19, "nineteen")
      (20, "twenty")
      (30, "thirty")
      (40, "forty")
      (50, "fifty")
      (60, "sixty")
      (70, "seventy")
      (80, "eighty")
      (90, "ninety") ]
    |> Map.ofList


let rec numberToWord acc number =
    if number > 1000 then
        failwith "does not support numbers larger than 1000"
    else if number = 1000 then
        "onethousand"
    else if number >= 100 then
        let leadingNumber = number / 100
        let reminder = number % 100
        let word = Map.find leadingNumber numberToWordMap + "hundred"

        if reminder = 0 then
            acc + word
        else
            numberToWord (acc + word + "and") reminder
    else
        match Map.tryFind number numberToWordMap with
        | Some word -> acc + word
        | None ->
            let leadingNumber = (number / 10) * 10
            let reminder = number % 10
            let word = Map.find leadingNumber numberToWordMap
            numberToWord (acc + word) reminder


let letterCount max =
    seq { 1..max }
    |> Seq.map (fun n -> numberToWord "" n)
    |> Seq.map Seq.length
    |> Seq.sum

let solution =
    { number = 17
      value = Int <| letterCount 1000 }
