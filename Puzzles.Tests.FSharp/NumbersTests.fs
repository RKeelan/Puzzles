module Puzzles.Tests.FSharp.NumbersTests

open NUnit.Framework
open Util
open Numbers

[<Test>]
let numDigits () =
    Assert.AreEqual(1, Numbers.numDigits 0)
    Assert.AreEqual(1, Numbers.numDigits 9)

    Assert.AreEqual(2, Numbers.numDigits 10)
    Assert.AreEqual(2, Numbers.numDigits 19)

    Assert.AreEqual(3, Numbers.numDigits 100)
    Assert.AreEqual(3, Numbers.numDigits 999)

    Assert.AreEqual(4, Numbers.numDigits 1000)
    Assert.AreEqual(4, Numbers.numDigits 9999)

    Assert.AreEqual(5, Numbers.numDigits 10000)
    Assert.AreEqual(5, Numbers.numDigits 99999)

    Assert.AreEqual(6, Numbers.numDigits 100000)
    Assert.AreEqual(6, Numbers.numDigits 999999)

    Assert.AreEqual(7, Numbers.numDigits 1000000)
    Assert.AreEqual(7, Numbers.numDigits 9999999)

    Assert.AreEqual(8, Numbers.numDigits 10000000)
    Assert.AreEqual(8, Numbers.numDigits 99999999)

    Assert.AreEqual(9, Numbers.numDigits 100000000)
    Assert.AreEqual(9, Numbers.numDigits 999999999)

    Assert.AreEqual(10, Numbers.numDigits 1000000000)
    Assert.AreEqual(10, Numbers.numDigits 2000000000)

[<Test>]
let lowestMultiple () =
    Assert.AreEqual((2, 2), (lowestMultiple 1 2))
    Assert.AreEqual((2, 1), (lowestMultiple 2 3))
    Assert.AreEqual((12, 3), (lowestMultiple 4 15))
    Assert.AreEqual((24, 4), (lowestMultiple 6 24))

[<Test>]
let factorial () =
    Assert.AreEqual(1, factorial 0)
    Assert.AreEqual(1, factorial 1)
    Assert.AreEqual(2, factorial 2)
    Assert.AreEqual(6, factorial 3)

[<Test>]
let summands () =
    Assert.AreEqual(seq { (0,1)}, summands 1)
    Assert.AreEqual(seq { (0,2); (1,1)}, summands 2)
    Assert.AreEqual(seq { (0,3); (1,2)}, summands 3)
    Assert.AreEqual(seq { (0,4); (1,3); (2,2)}, summands 4)

[<Test>]
let isDivisible () =
    Assert.IsTrue(isDivisible 1 1)
    Assert.IsTrue(isDivisible 4 2)
    Assert.IsTrue(isDivisible 6 3)
    Assert.IsTrue(isDivisible 10 2)

[<Test>]
let divisors () =
    Assert.AreEqual([1], List.sort(Seq.toList(divisors 1)))
    Assert.AreEqual([1; 2], List.sort(Seq.toList(divisors 2)))
    Assert.AreEqual([1; 3], List.sort(Seq.toList(divisors 3)))
    Assert.AreEqual([1; 2; 4], List.sort(Seq.toList(divisors 4)))
    Assert.AreEqual([1; 5], List.sort(Seq.toList(divisors 5)))
    Assert.AreEqual([1; 2; 3; 6], List.sort(Seq.toList(divisors 6)))
    Assert.AreEqual([1; 7], List.sort(Seq.toList(divisors 7)))
    Assert.AreEqual([1; 2; 4; 8], List.sort(Seq.toList(divisors 8)))
    Assert.AreEqual([1; 3; 9], List.sort(Seq.toList(divisors 9)))

    Assert.AreEqual([1; 2; 5; 10], List.sort(Seq.toList(divisors 10)))
    Assert.AreEqual([1; 3; 5; 15], List.sort(Seq.toList(divisors 15)))
    Assert.AreEqual([1; 3; 7; 21], List.sort(Seq.toList(divisors 21)))
    Assert.AreEqual([1; 2; 4; 7; 14; 28], List.sort(Seq.toList(divisors 28)))

    Assert.AreEqual([1; 2; 4; 5; 10; 20; 25; 50; 100], List.sort(Seq.toList(divisors 100)))

[<Test>]
let factors () =
    Assert.AreEqual([(1,1)], Seq.toList(factors 1))
    Assert.AreEqual([(1,2)], Seq.toList(factors 2))
    Assert.AreEqual([(1,3)], Seq.toList(factors 3))
    Assert.AreEqual([(1,4);(2,2)], Seq.toList(factors 4))
    Assert.AreEqual([(1,5)], Seq.toList(factors 5))
    Assert.AreEqual([(1,6);(2,3)], Seq.toList(factors 6))
    Assert.AreEqual([(1,7)], Seq.toList(factors 7))
    Assert.AreEqual([(1,8);(2,4)], Seq.toList(factors 8))
    Assert.AreEqual([(1,9);(3,3)], Seq.toList(factors 9))
    
    Assert.AreEqual([(1,10);(2,5)], Seq.toList(factors 10))
    Assert.AreEqual([(1,15);(3,5)], Seq.toList(factors 15))
    Assert.AreEqual([(1,21);(3,7)], Seq.toList(factors 21))
    Assert.AreEqual([(1,28);(2,14);(4,7)], Seq.toList(factors 28))
    
    Assert.AreEqual([(1,100);(2,50);(4,25);(5,20);(10,10)], Seq.toList(factors 100))

[<Test>]
let product () =
    let int32Product = [1; 2; 3; 4] |> product
    Assert.AreEqual(1*2*3*4, int32Product)

    // RK 15-Dec-2021 See comment on Util.product
    //let int64Product = [1000L; 2000L; 3000L; 4000L] |> Util.product
    //Assert.AreEqual(1000L*2000L*3000L*4000L, int64Product)
    
[<Test>]
let isInteger () =
    Assert.IsTrue(isInteger 1.0)
    Assert.IsTrue(isInteger 2.0)
    
    Assert.IsFalse(isInteger 0.5)
    Assert.IsFalse(isInteger 1.5)

[<Test>]
let isTriangleNumber () =
    Assert.IsFalse(isTriangleNumber 0)
    Assert.IsTrue(isTriangleNumber 1)
    Assert.IsFalse(isTriangleNumber 2)
    Assert.IsTrue(isTriangleNumber 3)
    Assert.IsFalse(isTriangleNumber 4)
    
    Assert.IsFalse(isTriangleNumber 5)
    Assert.IsTrue(isTriangleNumber 6)
    Assert.IsFalse(isTriangleNumber 7)
    
    Assert.IsFalse(isTriangleNumber 9)
    Assert.IsTrue(isTriangleNumber 10)
    Assert.IsFalse(isTriangleNumber 11)
    
    Assert.IsFalse(isTriangleNumber 14)
    Assert.IsTrue(isTriangleNumber 15)
    Assert.IsFalse(isTriangleNumber 16)
    
    Assert.IsFalse(isTriangleNumber 20)
    Assert.IsTrue(isTriangleNumber 21)
    Assert.IsFalse(isTriangleNumber 22)
    
    Assert.IsFalse(isTriangleNumber 27)
    Assert.IsTrue(isTriangleNumber 28)
    Assert.IsFalse(isTriangleNumber 29)
    
    Assert.IsFalse(isTriangleNumber 35)
    Assert.IsTrue(isTriangleNumber 36)
    Assert.IsFalse(isTriangleNumber 37)
    
    Assert.IsFalse(isTriangleNumber 44)
    Assert.IsTrue(isTriangleNumber 45)
    Assert.IsFalse(isTriangleNumber 46)
    
    Assert.IsFalse(isTriangleNumber 54)
    Assert.IsTrue(isTriangleNumber 55)
    Assert.IsFalse(isTriangleNumber 56)
    
[<Test>]
let triangleNumber () =
    Assert.AreEqual(1, triangleNumber 1)
    Assert.AreEqual(3, triangleNumber 2)
    Assert.AreEqual(6, triangleNumber 3)
    Assert.AreEqual(10, triangleNumber 4)
    Assert.AreEqual(15, triangleNumber 5)
    Assert.AreEqual(21, triangleNumber 6)
    Assert.AreEqual(28, triangleNumber 7)
    Assert.AreEqual(36, triangleNumber 8)
    Assert.AreEqual(45, triangleNumber 9)
    Assert.AreEqual(55, triangleNumber 10)
    
    Assert.AreEqual(40755, triangleNumber 285)
    
[<Test>]
let isPentagonNumber () =
    Assert.IsFalse(isPentagonNumber 0)
    Assert.IsTrue(isPentagonNumber 1)
    Assert.IsFalse(isPentagonNumber 2)
    
    Assert.IsFalse(isPentagonNumber 4)
    Assert.IsTrue(isPentagonNumber 5)
    Assert.IsFalse(isPentagonNumber 6)
    
    Assert.IsFalse(isPentagonNumber 11)
    Assert.IsTrue(isPentagonNumber 12)
    Assert.IsFalse(isPentagonNumber 13)
    
    Assert.IsFalse(isPentagonNumber 21)
    Assert.IsTrue(isPentagonNumber 22)
    Assert.IsFalse(isPentagonNumber 23)
    
    Assert.IsFalse(isPentagonNumber 34)
    Assert.IsTrue(isPentagonNumber 35)
    Assert.IsFalse(isPentagonNumber 36)
    
    Assert.IsFalse(isPentagonNumber 50)
    Assert.IsTrue(isPentagonNumber 51)
    Assert.IsFalse(isPentagonNumber 52)
    
    Assert.IsFalse(isPentagonNumber 69)
    Assert.IsTrue(isPentagonNumber 70)
    Assert.IsFalse(isPentagonNumber 71)
    
    Assert.IsFalse(isPentagonNumber 91)
    Assert.IsTrue(isPentagonNumber 92)
    Assert.IsFalse(isPentagonNumber 93)
    
    Assert.IsFalse(isPentagonNumber 116)
    Assert.IsTrue(isPentagonNumber 117)
    Assert.IsFalse(isPentagonNumber 118)
    
    Assert.IsFalse(isPentagonNumber 144)
    Assert.IsTrue(isPentagonNumber 145)
    Assert.IsFalse(isPentagonNumber 146)
    
[<Test>]
let pentagonNumber () =
    Assert.AreEqual(1, pentagonNumber 1)
    Assert.AreEqual(5, pentagonNumber 2)
    Assert.AreEqual(12, pentagonNumber 3)
    Assert.AreEqual(22, pentagonNumber 4)
    Assert.AreEqual(35, pentagonNumber 5)
    Assert.AreEqual(51, pentagonNumber 6)
    Assert.AreEqual(70, pentagonNumber 7)
    Assert.AreEqual(92, pentagonNumber 8)
    Assert.AreEqual(117, pentagonNumber 9)
    Assert.AreEqual(145, pentagonNumber 10)
    
    Assert.AreEqual(40755, pentagonNumber 165)
    
[<Test>]
let isHexagonNumber () =
    Assert.IsFalse(isHexagonNumber 0)
    Assert.IsTrue(isHexagonNumber 1)
    Assert.IsFalse(isHexagonNumber 2)
    
    Assert.IsFalse(isHexagonNumber 40754)
    Assert.IsTrue(isHexagonNumber 40755)
    Assert.IsFalse(isHexagonNumber 40756)
    
[<Test>]
let hexagonNumber () =
    Assert.AreEqual(1, hexagonNumber 1)
    Assert.AreEqual(40755, hexagonNumber 143)