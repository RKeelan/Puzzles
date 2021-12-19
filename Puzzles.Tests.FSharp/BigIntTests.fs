module Puzzles.Tests.FSharp.BigIntTests

open NUnit.Framework
open BigInt

[<Test>]
let ctor () =
    let mutable bigInt = BigInt(1L, 10L)
    Assert.AreEqual("1", bigInt.ToString())

    bigInt <- BigInt(11L, 10L)
    Assert.AreEqual("11", bigInt.ToString())
    
    bigInt <- BigInt(1000L, BigInt.INT_32_RADIX)
    Assert.AreEqual("1000", bigInt.ToString())
    
    bigInt <- BigInt(12345678901L, BigInt.INT_32_RADIX)
    Assert.AreEqual("12345678901", bigInt.ToString())
    
    bigInt <- BigInt(12345678901L, BigInt.INT_64_RADIX)
    Assert.AreEqual("12345678901", bigInt.ToString())
    
    bigInt <- BigInt(123456789987654L, BigInt.INT_64_RADIX)
    Assert.AreEqual("123456789987654", bigInt.ToString())

[<Test>]
let toString () =
    let mutable bigInt = BigInt(101L, 10L)
    Assert.AreEqual("101", bigInt.ToString())

    bigInt <- BigInt(1001L, 100L)
    Assert.AreEqual("1001", bigInt.ToString())
    
    bigInt <- BigInt(10001L, 1000L)
    Assert.AreEqual("10001", bigInt.ToString())
    
    bigInt <- BigInt(100001L, 10000L)
    Assert.AreEqual("100001", bigInt.ToString())
    
    bigInt <- BigInt(1000001L, 100000L)
    Assert.AreEqual("1000001", bigInt.ToString())
    
    bigInt <- BigInt(10000001L, 1000000L)
    Assert.AreEqual("10000001", bigInt.ToString())
    
    bigInt <- BigInt(100000001L, 10000000L)
    Assert.AreEqual("100000001", bigInt.ToString())
    
    bigInt <- BigInt(1000000001L, 100000000L)
    Assert.AreEqual("1000000001", bigInt.ToString())
    
    bigInt <- BigInt(10000000001L, 1000000000L)
    Assert.AreEqual("10000000001", bigInt.ToString())
    
    bigInt <- BigInt(100000000001L, 10000000000L)
    Assert.AreEqual("100000000001", bigInt.ToString())
    
    bigInt <- BigInt(1000000000001L, 100000000000L)
    Assert.AreEqual("1000000000001", bigInt.ToString())
    
    bigInt <- BigInt(10000000000001L, 1000000000000L)
    Assert.AreEqual("10000000000001", bigInt.ToString())
    
    bigInt <- BigInt(100000000000001L, 10000000000000L)
    Assert.AreEqual("100000000000001", bigInt.ToString())
    
    bigInt <- BigInt(1000000000000001L, 100000000000000L)
    Assert.AreEqual("1000000000000001", bigInt.ToString())
    
    bigInt <- BigInt(10000000000000001L, 1000000000000000L)
    Assert.AreEqual("10000000000000001", bigInt.ToString())
    
    bigInt <- BigInt(100000000000000001L, 10000000000000000L)
    Assert.AreEqual("100000000000000001", bigInt.ToString())
    
    bigInt <- BigInt(1000000000000000001L, 100000000000000000L)
    Assert.AreEqual("1000000000000000001", bigInt.ToString())
    
    bigInt <- BigInt(1000000000000000001L, 10L)
    Assert.AreEqual("1000000000000000001", bigInt.ToString())

[<Test>]
let multiply () =
    let mutable bigInt = BigInt(6L, 10L)
    bigInt <- bigInt.multiply(2L)
    Assert.AreEqual("12", bigInt.ToString())
        
[<Test>]
let double () =
    let rec double (number : BigInt) (n : int64) =
        let numberDoubled = number.multiply(2L)
        match n with
        | 1L -> numberDoubled
        | _ -> double numberDoubled (n - 1L)
    let bigInt = double (new BigInt(1L, BigInt.INT_32_RADIX)) 1000L
    let bigResult = "107150860718626732094842504906000181056140481170553360744375038837035105112" +
                    "493612249319837881569585812759467291755314682518714528569231404359845775746" +
                    "985748039345677748242309854210746050623711418779541821530464749835819412673" +
                    "98767559165543946077062914571196477686542167660429831652624386837205668069376"
    Assert.AreEqual(bigResult, bigInt.ToString())

