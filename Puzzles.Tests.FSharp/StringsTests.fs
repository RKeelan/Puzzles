module Puzzles.Tests.FSharp.StringsTests

open System
open NUnit.Framework
open NUnit.Framework.Legacy
open Strings

[<Test>]
let hasDuplicates () =
    ClassicAssert.IsTrue(hasDuplicates "1123456790")
    ClassicAssert.IsTrue(hasDuplicates "111111")
    ClassicAssert.IsTrue(hasDuplicates "11")
    ClassicAssert.IsTrue(hasDuplicates "1122")
    ClassicAssert.IsTrue(hasDuplicates "aab")
    ClassicAssert.IsTrue(hasDuplicates "abb")
    ClassicAssert.IsTrue(hasDuplicates "abbc")

    ClassicAssert.IsFalse(hasDuplicates "1234568790")
    ClassicAssert.IsFalse(hasDuplicates "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuv")
    
[<Test>]
let reverse () =
    ClassicAssert.AreEqual("1", (reverse "1"))
    ClassicAssert.AreEqual("12", (reverse "21"))
    ClassicAssert.AreEqual("123", (reverse "321"))
    
[<Test>]
let rotate () =
    ClassicAssert.AreEqual("001", (rotate "100"))
    
[<Test>]
let rotations () =
    ClassicAssert.AreEqual(["100";"001";"010"], (rotations "100"))
    
[<Test>]
let isPalindrome () =
    ClassicAssert.IsTrue(isPalindrome "abba")
    ClassicAssert.IsTrue(isPalindrome "abcba")
    
[<Test>]
let isPandigital () =
    ClassicAssert.IsTrue(isPandigital "1" 1)
    ClassicAssert.IsTrue(isPandigital "21" 2)
    ClassicAssert.IsTrue(isPandigital "132" 3)
    ClassicAssert.IsTrue(isPandigital "4132" 4)
    ClassicAssert.IsTrue(isPandigital "41532" 5)
    ClassicAssert.IsTrue(isPandigital "415362" 6)
    ClassicAssert.IsTrue(isPandigital "7415362" 7)
    ClassicAssert.IsTrue(isPandigital "74153628" 8)
    ClassicAssert.IsTrue(isPandigital "974153628" 9)
    
    ClassicAssert.IsFalse(isPandigital "074153628" 9)
    ClassicAssert.IsFalse(isPandigital "9974153628" 9)
    ClassicAssert.IsFalse(isPandigital "74153628" 9)

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
    ClassicAssert.AreEqual(expected, p)

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
    ClassicAssert.AreEqual(expected, p)