module Puzzles.Tests.FSharp.StringsTests

open System
open NUnit.Framework
open Strings

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
    Assert.IsTrue(isPalindrome "abba")
    Assert.IsTrue(isPalindrome "abcba")
    
[<Test>]
let isPandigital () =
    Assert.IsTrue(isPandigital "1" 1)
    Assert.IsTrue(isPandigital "21" 2)
    Assert.IsTrue(isPandigital "132" 3)
    Assert.IsTrue(isPandigital "4132" 4)
    Assert.IsTrue(isPandigital "41532" 5)
    Assert.IsTrue(isPandigital "415362" 6)
    Assert.IsTrue(isPandigital "7415362" 7)
    Assert.IsTrue(isPandigital "74153628" 8)
    Assert.IsTrue(isPandigital "974153628" 9)
    
    Assert.IsFalse(isPandigital "074153628" 9)
    Assert.IsFalse(isPandigital "9974153628" 9)
    Assert.IsFalse(isPandigital "74153628" 9)

[<Test>]
let lexicographicPermutations () =
    let mutable expected = [
        "123";
        "132";
        "213";
        "231";
        "312";
        "321"
    ]
    let mutable p = lexicographicPermutations "321"
    Assert.AreEqual(expected, p)

    expected <- [
        "1234";
        "1243";
        "1324";
        "1342";
        "1423";
        "1432";
        "2134";
        "2143";
        "2314";
        "2341";
        "2413";
        "2431";
        "3124";
        "3142";
        "3214";
        "3241";
        "3412";
        "3421";
        "4123";
        "4132";
        "4213";
        "4231";
        "4312";
        "4321"
    ]
    p <- lexicographicPermutations "3241"
    printfn "%s" (String.Join("\n", p))
    Assert.AreEqual(expected, p)