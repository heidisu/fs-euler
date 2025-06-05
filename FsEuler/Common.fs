module FsEuler.Common

let rec factorize n next acc=
    if next > n then acc
    else
        if n % next = 0L then factorize (n / next) next (next :: acc)
        else factorize n (next + 1L) acc