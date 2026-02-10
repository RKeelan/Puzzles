module Puzzles.Tests.FSharp.CombinatronicsTests

open System
open NUnit.Framework
open NUnit.Framework.Legacy

open System

[<Test>]
let words () =
    let abAlphabet = set ['A';'B']

    let mutable expected = seq {
        ['A'];
        ['B']
    }
    let mutable actual = Combinatronics.words abAlphabet 1
    ClassicAssert.AreEqual(expected, actual)
    
    expected <- seq {
        ['A';'A'];
        ['A';'B'];
        ['B';'A'];
        ['B';'B']
    }
    actual <- Combinatronics.words abAlphabet 2
    ClassicAssert.AreEqual(expected, actual)
    ClassicAssert.AreEqual(expected, actual)
    
    expected <- seq {
        ['A';'A';'A'];
        ['A';'A';'B'];
        ['A';'B';'A'];
        ['A';'B';'B'];
        ['B';'A';'A'];
        ['B';'A';'B'];
        ['B';'B';'A']
        ['B';'B';'B']
    }
    actual <- Combinatronics.words abAlphabet 3
    ClassicAssert.AreEqual(expected, actual)