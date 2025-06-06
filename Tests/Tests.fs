module Tests

open Xunit
open FsEuler

[<Fact>]
let ``P001 sum of 3 and 5 for numbers below 10`` () = Assert.Equal(23, P001.sum3and5 10)

[<Fact>]
let ``P002 fibonnaci numbers less than 100`` () =
    Assert.Equal([ 1; 2; 3; 5; 8; 13; 21; 34; 55; 89 ], P002.fib 100)

[<Fact>]
let ``prime factors of 13195`` () =
    let actual = Common.factorize 13195L 2L []
    Assert.Equal<int64>([ 29L; 13L; 7L; 5L ], actual)

[<Fact>]
let ``P003 max factor of 13195`` () =
    Assert.Equal(29L, P003.maxFactor 13195L)

[<Fact>]
let ``P004 largest palindrom as product of two digit numbers i 9009`` () =
    Assert.Equal(9009, P004.maxPalindrome 2)

[<Fact>]
let ``P005 smallest number divisible 1 .. 10`` () =
    Assert.Equal(2520L, P005.smallestDivisibleByAllNumbers 10l)

[<Fact>]
let ``P006 sum square difference of ten first digits`` () =
    Assert.Equal(2640, P006.squareSumDifference2 10)

[<Fact>]
let ``P007 6th prime is 13`` () = Assert.Equal(13, P007.nthPrime 6)

[<Fact>]
let ``P008 largest product of 4 adjacent digits`` () =
    Assert.Equal(5832L, P008.biggestAdjacentProduct 4)
