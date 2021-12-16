module Puzzles.Tests.FSharp.UtilTests

open System
open System.Linq
open NUnit.Framework
open Util
open Humanizer

[<Test>]
let divisorsTest () =
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
let productTest () =
    let int32Product = [1; 2; 3; 4] |> Util.product
    Assert.AreEqual(1*2*3*4, int32Product)

    // RK 15-Dec-2021 See comment on Util.product
    //let int64Product = [1000L; 2000L; 3000L; 4000L] |> Util.product
    //Assert.AreEqual(1000L*2000L*3000L*4000L, int64Product)