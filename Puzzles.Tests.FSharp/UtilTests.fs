module Puzzles.Tests.FSharp.UtilTests

open NUnit.Framework
open Util

[<Test>]
let SeqProductTest () =
    let int32Product = [1; 2; 3; 4] |> Util.product
    Assert.AreEqual(1*2*3*4, int32Product)

    // RK 15-Dec-2021 See comment on Util.product
    //let int64Product = [1000L; 2000L; 3000L; 4000L] |> Util.product
    //Assert.AreEqual(1000L*2000L*3000L*4000L, int64Product)

