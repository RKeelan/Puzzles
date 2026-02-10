module Puzzles.Tests.FSharp.NumbersTests

open NUnit.Framework
open NUnit.Framework.Legacy
open Util
open Numbers

[<Test>]
let numDigits () =
    ClassicAssert.AreEqual(1, Numbers.numDigits 0)
    ClassicAssert.AreEqual(1, Numbers.numDigits 9)

    ClassicAssert.AreEqual(2, Numbers.numDigits 10)
    ClassicAssert.AreEqual(2, Numbers.numDigits 19)

    ClassicAssert.AreEqual(3, Numbers.numDigits 100)
    ClassicAssert.AreEqual(3, Numbers.numDigits 999)

    ClassicAssert.AreEqual(4, Numbers.numDigits 1000)
    ClassicAssert.AreEqual(4, Numbers.numDigits 9999)

    ClassicAssert.AreEqual(5, Numbers.numDigits 10000)
    ClassicAssert.AreEqual(5, Numbers.numDigits 99999)

    ClassicAssert.AreEqual(6, Numbers.numDigits 100000)
    ClassicAssert.AreEqual(6, Numbers.numDigits 999999)

    ClassicAssert.AreEqual(7, Numbers.numDigits 1000000)
    ClassicAssert.AreEqual(7, Numbers.numDigits 9999999)

    ClassicAssert.AreEqual(8, Numbers.numDigits 10000000)
    ClassicAssert.AreEqual(8, Numbers.numDigits 99999999)

    ClassicAssert.AreEqual(9, Numbers.numDigits 100000000)
    ClassicAssert.AreEqual(9, Numbers.numDigits 999999999)

    ClassicAssert.AreEqual(10, Numbers.numDigits 1000000000)
    ClassicAssert.AreEqual(10, Numbers.numDigits 2000000000)

[<Test>]
let lowestMultiple () =
    ClassicAssert.AreEqual((2, 2), (lowestMultiple 1 2))
    ClassicAssert.AreEqual((2, 1), (lowestMultiple 2 3))
    ClassicAssert.AreEqual((12, 3), (lowestMultiple 4 15))
    ClassicAssert.AreEqual((24, 4), (lowestMultiple 6 24))

[<Test>]
let factorial () =
    ClassicAssert.AreEqual(1, factorial 0)
    ClassicAssert.AreEqual(1, factorial 1)
    ClassicAssert.AreEqual(2, factorial 2)
    ClassicAssert.AreEqual(6, factorial 3)

[<Test>]
let summands () =
    ClassicAssert.AreEqual(seq { (0,1)}, summands 1)
    ClassicAssert.AreEqual(seq { (0,2); (1,1)}, summands 2)
    ClassicAssert.AreEqual(seq { (0,3); (1,2)}, summands 3)
    ClassicAssert.AreEqual(seq { (0,4); (1,3); (2,2)}, summands 4)

[<Test>]
let isDivisible () =
    ClassicAssert.IsTrue(isDivisible 1 1)
    ClassicAssert.IsTrue(isDivisible 4 2)
    ClassicAssert.IsTrue(isDivisible 6 3)
    ClassicAssert.IsTrue(isDivisible 10 2)

[<Test>]
let divisors () =
    ClassicAssert.AreEqual([1], List.sort(Seq.toList(divisors 1)))
    ClassicAssert.AreEqual([1; 2], List.sort(Seq.toList(divisors 2)))
    ClassicAssert.AreEqual([1; 3], List.sort(Seq.toList(divisors 3)))
    ClassicAssert.AreEqual([1; 2; 4], List.sort(Seq.toList(divisors 4)))
    ClassicAssert.AreEqual([1; 5], List.sort(Seq.toList(divisors 5)))
    ClassicAssert.AreEqual([1; 2; 3; 6], List.sort(Seq.toList(divisors 6)))
    ClassicAssert.AreEqual([1; 7], List.sort(Seq.toList(divisors 7)))
    ClassicAssert.AreEqual([1; 2; 4; 8], List.sort(Seq.toList(divisors 8)))
    ClassicAssert.AreEqual([1; 3; 9], List.sort(Seq.toList(divisors 9)))

    ClassicAssert.AreEqual([1; 2; 5; 10], List.sort(Seq.toList(divisors 10)))
    ClassicAssert.AreEqual([1; 3; 5; 15], List.sort(Seq.toList(divisors 15)))
    ClassicAssert.AreEqual([1; 3; 7; 21], List.sort(Seq.toList(divisors 21)))
    ClassicAssert.AreEqual([1; 2; 4; 7; 14; 28], List.sort(Seq.toList(divisors 28)))

    ClassicAssert.AreEqual([1; 2; 4; 5; 10; 20; 25; 50; 100], List.sort(Seq.toList(divisors 100)))

[<Test>]
let factors () =
    ClassicAssert.AreEqual([(1,1)], Seq.toList(factors 1))
    ClassicAssert.AreEqual([(1,2)], Seq.toList(factors 2))
    ClassicAssert.AreEqual([(1,3)], Seq.toList(factors 3))
    ClassicAssert.AreEqual([(1,4);(2,2)], Seq.toList(factors 4))
    ClassicAssert.AreEqual([(1,5)], Seq.toList(factors 5))
    ClassicAssert.AreEqual([(1,6);(2,3)], Seq.toList(factors 6))
    ClassicAssert.AreEqual([(1,7)], Seq.toList(factors 7))
    ClassicAssert.AreEqual([(1,8);(2,4)], Seq.toList(factors 8))
    ClassicAssert.AreEqual([(1,9);(3,3)], Seq.toList(factors 9))
    
    ClassicAssert.AreEqual([(1,10);(2,5)], Seq.toList(factors 10))
    ClassicAssert.AreEqual([(1,15);(3,5)], Seq.toList(factors 15))
    ClassicAssert.AreEqual([(1,21);(3,7)], Seq.toList(factors 21))
    ClassicAssert.AreEqual([(1,28);(2,14);(4,7)], Seq.toList(factors 28))
    
    ClassicAssert.AreEqual([(1,100);(2,50);(4,25);(5,20);(10,10)], Seq.toList(factors 100))

[<Test>]
let product () =
    let int32Product = [1; 2; 3; 4] |> product
    ClassicAssert.AreEqual(1*2*3*4, int32Product)

    // RK 15-Dec-2021 See comment on Util.product
    //let int64Product = [1000L; 2000L; 3000L; 4000L] |> Util.product
    //ClassicAssert.AreEqual(1000L*2000L*3000L*4000L, int64Product)
    
[<Test>]
let isInteger () =
    ClassicAssert.IsTrue(isInteger 1.0)
    ClassicAssert.IsTrue(isInteger 2.0)
    
    ClassicAssert.IsFalse(isInteger 0.5)
    ClassicAssert.IsFalse(isInteger 1.5)

[<Test>]
let isTriangleNumber () =
    ClassicAssert.IsFalse(isTriangleNumber 0)
    ClassicAssert.IsTrue(isTriangleNumber 1)
    ClassicAssert.IsFalse(isTriangleNumber 2)
    ClassicAssert.IsTrue(isTriangleNumber 3)
    ClassicAssert.IsFalse(isTriangleNumber 4)
    
    ClassicAssert.IsFalse(isTriangleNumber 5)
    ClassicAssert.IsTrue(isTriangleNumber 6)
    ClassicAssert.IsFalse(isTriangleNumber 7)
    
    ClassicAssert.IsFalse(isTriangleNumber 9)
    ClassicAssert.IsTrue(isTriangleNumber 10)
    ClassicAssert.IsFalse(isTriangleNumber 11)
    
    ClassicAssert.IsFalse(isTriangleNumber 14)
    ClassicAssert.IsTrue(isTriangleNumber 15)
    ClassicAssert.IsFalse(isTriangleNumber 16)
    
    ClassicAssert.IsFalse(isTriangleNumber 20)
    ClassicAssert.IsTrue(isTriangleNumber 21)
    ClassicAssert.IsFalse(isTriangleNumber 22)
    
    ClassicAssert.IsFalse(isTriangleNumber 27)
    ClassicAssert.IsTrue(isTriangleNumber 28)
    ClassicAssert.IsFalse(isTriangleNumber 29)
    
    ClassicAssert.IsFalse(isTriangleNumber 35)
    ClassicAssert.IsTrue(isTriangleNumber 36)
    ClassicAssert.IsFalse(isTriangleNumber 37)
    
    ClassicAssert.IsFalse(isTriangleNumber 44)
    ClassicAssert.IsTrue(isTriangleNumber 45)
    ClassicAssert.IsFalse(isTriangleNumber 46)
    
    ClassicAssert.IsFalse(isTriangleNumber 54)
    ClassicAssert.IsTrue(isTriangleNumber 55)
    ClassicAssert.IsFalse(isTriangleNumber 56)
    
[<Test>]
let triangleNumber () =
    ClassicAssert.AreEqual(1, triangleNumber 1)
    ClassicAssert.AreEqual(3, triangleNumber 2)
    ClassicAssert.AreEqual(6, triangleNumber 3)
    ClassicAssert.AreEqual(10, triangleNumber 4)
    ClassicAssert.AreEqual(15, triangleNumber 5)
    ClassicAssert.AreEqual(21, triangleNumber 6)
    ClassicAssert.AreEqual(28, triangleNumber 7)
    ClassicAssert.AreEqual(36, triangleNumber 8)
    ClassicAssert.AreEqual(45, triangleNumber 9)
    ClassicAssert.AreEqual(55, triangleNumber 10)
    
    ClassicAssert.AreEqual(40755, triangleNumber 285)
    
[<Test>]
let isPentagonNumber () =
    ClassicAssert.IsFalse(isPentagonNumber 0)
    ClassicAssert.IsTrue(isPentagonNumber 1)
    ClassicAssert.IsFalse(isPentagonNumber 2)
    
    ClassicAssert.IsFalse(isPentagonNumber 4)
    ClassicAssert.IsTrue(isPentagonNumber 5)
    ClassicAssert.IsFalse(isPentagonNumber 6)
    
    ClassicAssert.IsFalse(isPentagonNumber 11)
    ClassicAssert.IsTrue(isPentagonNumber 12)
    ClassicAssert.IsFalse(isPentagonNumber 13)
    
    ClassicAssert.IsFalse(isPentagonNumber 21)
    ClassicAssert.IsTrue(isPentagonNumber 22)
    ClassicAssert.IsFalse(isPentagonNumber 23)
    
    ClassicAssert.IsFalse(isPentagonNumber 34)
    ClassicAssert.IsTrue(isPentagonNumber 35)
    ClassicAssert.IsFalse(isPentagonNumber 36)
    
    ClassicAssert.IsFalse(isPentagonNumber 50)
    ClassicAssert.IsTrue(isPentagonNumber 51)
    ClassicAssert.IsFalse(isPentagonNumber 52)
    
    ClassicAssert.IsFalse(isPentagonNumber 69)
    ClassicAssert.IsTrue(isPentagonNumber 70)
    ClassicAssert.IsFalse(isPentagonNumber 71)
    
    ClassicAssert.IsFalse(isPentagonNumber 91)
    ClassicAssert.IsTrue(isPentagonNumber 92)
    ClassicAssert.IsFalse(isPentagonNumber 93)
    
    ClassicAssert.IsFalse(isPentagonNumber 116)
    ClassicAssert.IsTrue(isPentagonNumber 117)
    ClassicAssert.IsFalse(isPentagonNumber 118)
    
    ClassicAssert.IsFalse(isPentagonNumber 144)
    ClassicAssert.IsTrue(isPentagonNumber 145)
    ClassicAssert.IsFalse(isPentagonNumber 146)
    
[<Test>]
let pentagonNumber () =
    ClassicAssert.AreEqual(1, pentagonNumber 1)
    ClassicAssert.AreEqual(5, pentagonNumber 2)
    ClassicAssert.AreEqual(12, pentagonNumber 3)
    ClassicAssert.AreEqual(22, pentagonNumber 4)
    ClassicAssert.AreEqual(35, pentagonNumber 5)
    ClassicAssert.AreEqual(51, pentagonNumber 6)
    ClassicAssert.AreEqual(70, pentagonNumber 7)
    ClassicAssert.AreEqual(92, pentagonNumber 8)
    ClassicAssert.AreEqual(117, pentagonNumber 9)
    ClassicAssert.AreEqual(145, pentagonNumber 10)
    
    ClassicAssert.AreEqual(40755, pentagonNumber 165)
    
[<Test>]
let isHexagonNumber () =
    ClassicAssert.IsFalse(isHexagonNumber 0)
    ClassicAssert.IsTrue(isHexagonNumber 1)
    ClassicAssert.IsFalse(isHexagonNumber 2)
    
    ClassicAssert.IsFalse(isHexagonNumber 40754)
    ClassicAssert.IsTrue(isHexagonNumber 40755)
    ClassicAssert.IsFalse(isHexagonNumber 40756)
    
[<Test>]
let hexagonNumber () =
    ClassicAssert.AreEqual(1, hexagonNumber 1)
    ClassicAssert.AreEqual(40755, hexagonNumber 143)