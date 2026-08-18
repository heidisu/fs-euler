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

[<Fact>]
let ``P009 special pythagorean triplets`` () =
    Assert.Equal(60, P009.specialPythagoreanTriplets 12)

[<Fact>]
let ``P010 sum of primes below 10`` () = Assert.Equal(17L, P010.primeSum2 10L)


[<Fact>]
let ``P011 max neighbour quartett`` () =
    let grid =
        """
14 02 03 04
05 15 07 08
09 10 11 12
13 01 06 16
"""

    Assert.Equal(36960, P011.maxProduct grid)

[<Fact>]
let ``P013 first ten digits of sum`` () =
    let numbers =
        """
1231231234
2312312311
"""

    Assert.Equal(3543543545L, P013.firstDigitsOfSum numbers)

[<Fact>]
let ``P014 collatzSequenceLength of 13 = 10`` () =
    Assert.Equal(10L, P014.collatzSequenceLength 13L 0L 13L)

[<Fact>]
let ``P014 number under 100 with max sequence number`` () =
    Assert.Equal(97L, P014.maxSequenceLength 100L)

[<Fact>]
let ``P015 number of paths in 2 x 2 grid equals 6`` () =
    Assert.Equal(6L, P015.betterSearch 2 4 [ (0, 0), 1L ])

[<Fact>]
let ``P016 sum of digits`` () =
    Assert.Equal(26I, P016.sumOfDigits 0I (pown 2I 15))

[<Fact>]
let ``P017 number to word`` () =
    Assert.Equal("threehundredandfortytwo", P017.numberToWord "" 342)

[<Fact>]
let ``P017 letterCount`` () = Assert.Equal(19, P017.letterCount 5)


let testdata =
    """
3
7 4
2 4 6
8 5 9 3
"""

[<Fact>]
let ``P018 find max path`` () =
    let data = P018.parse testdata
    Assert.Equal(P018.findMax data data[0] 1, 23)
