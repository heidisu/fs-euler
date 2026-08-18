module FsEuler.P018

open Types

let data =
    """
75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
"""

let parse (pyramidString: string) =
    pyramidString.Split(System.Environment.NewLine)
    |> Array.filter (fun l -> l <> "")
    |> Array.map (fun l -> l.Split(" ") |> Array.map int)

let rec findMax data result idx =
    if idx = Array.length data then
        Array.max result
    else
        let newResult =
            data[idx]
            |> Array.mapi (fun i v ->
                if i = 0 then result[0] + v
                else if i = idx then result[idx - 1] + v
                else max (result[i - 1] + v) (result[i] + v))

        findMax data newResult (idx + 1)

let getSolution () =
    let parsedData = parse data
    findMax parsedData parsedData[0] 1

let solution =
    { number = 18
      value = Int <| getSolution () }
