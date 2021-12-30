module Puzzles.Tests.FSharp.SearchTests

open System
open NUnit.Framework
open Numbers
open Search
open Euler44

[<Test>]
let aStarNodeEquality () =
    let a = {Cost = 1; Item = 1}
    let b = {Cost = 2; Item = 1}
    Assert.AreEqual(a,b)

    let pa = {Cost = 10; Item = new PentagonPair(1,2)}
    let pb = {Cost = 20; Item = new PentagonPair(1,2)}
    Assert.AreEqual(pa,pb)

    let map = Map.empty |> Map.add pa 20
    Assert.IsTrue(map.ContainsKey pb)
