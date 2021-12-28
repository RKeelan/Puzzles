module Puzzles.Tests.FSharp.UtilTests

open System
open System.Linq
open NUnit.Framework
open Util
open Humanizer

[<Test>]
let lowestMultiple () =
    Assert.AreEqual((2, 2), (Util.lowestMultiple 1 2))
    Assert.AreEqual((2, 1), (Util.lowestMultiple 2 3))
    Assert.AreEqual((12, 3), (Util.lowestMultiple 4 15))
    Assert.AreEqual((24, 4), (Util.lowestMultiple 6 24))

[<Test>]
let factorial () =
    Assert.AreEqual(0, Util.factorial 0)
    Assert.AreEqual(1, Util.factorial 1)
    Assert.AreEqual(2, Util.factorial 2)
    Assert.AreEqual(6, Util.factorial 3)

[<Test>]
let summands () =
    Assert.AreEqual(seq { (0,1)}, Util.summands 1)
    Assert.AreEqual(seq { (0,2); (1,1)}, Util.summands 2)
    Assert.AreEqual(seq { (0,3); (1,2)}, Util.summands 3)
    Assert.AreEqual(seq { (0,4); (1,3); (2,2)}, Util.summands 4)

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
    let int32Product = [1; 2; 3; 4] |> Util.product
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
let hasDuplicates () =
    Assert.IsTrue(hasDuplicates "1123456790")
    Assert.IsTrue(hasDuplicates "111111")
    Assert.IsTrue(hasDuplicates "11")
    Assert.IsTrue(hasDuplicates "1122")
    Assert.IsTrue(hasDuplicates "aab")
    Assert.IsTrue(hasDuplicates "abb")
    Assert.IsTrue(hasDuplicates "abbc")

    Assert.IsFalse(hasDuplicates "1234568790")
    Assert.IsFalse(hasDuplicates "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuv")
    
[<Test>]
let rotate () =
    Assert.AreEqual("001", (rotate "100"))
    
[<Test>]
let rotations () =
    Assert.AreEqual(["100";"001";"010"], (rotations "100"))
    
[<Test>]
let isPalindrome () =
    Assert.IsTrue(Util.isPalindrome "abba")
    Assert.IsTrue(Util.isPalindrome "abcba")