module Puzzles.Tests.FSharp.UtilTests

open System
open NUnit.Framework
open Util

[<Test>]
let uncheckedAddition () =
    // Signed Int32
    Assert.AreEqual(2, (1 +! 1))
    Assert.AreEqual(0x80000000, (1 +! System.Int32.MaxValue))
    
    // Signed Int64
    Assert.AreEqual(2L, (1L +! 1L))
    Assert.AreEqual(0x80000000L, (1L +! int64(System.Int32.MaxValue)))
    Assert.AreEqual(0x8000000000000000L, (1L +! System.Int64.MaxValue))

    // Unsigned Int32
    Assert.AreEqual(2u, (1u +! 1u))
    Assert.AreEqual(0u, (1u +! System.UInt32.MaxValue))
    
    // Unsigned Int64
    Assert.AreEqual(2UL, (1UL +! 1UL))
    Assert.AreEqual(0x100000000UL, (1UL +! uint64(System.UInt32.MaxValue)))
    Assert.AreEqual(0UL, (1UL +! System.UInt64.MaxValue))

[<Test>]
let uncheckedMultiplication () =
    // Signed Int32
    Assert.AreEqual(4, (2 *! 2))
    Assert.AreEqual(0xFFFFFFFE, (2 *! System.Int32.MaxValue))
    
    // Signed Int64
    Assert.AreEqual(16L, (4L *! 4L))
    Assert.AreEqual(0xFFFFFFFFFFFFFFFCL, (4L *! System.Int64.MaxValue))

    // Unsigned Int32
    Assert.AreEqual(9u, (3u *! 3u))
    Assert.AreEqual(4294967293u, (3u *! System.UInt32.MaxValue))
    
    // Unsigned Int64
    Assert.AreEqual(25UL, (5UL *! 5UL))
    Assert.AreEqual(18446744073709551611UL, (5UL *! System.UInt64.MaxValue))
    
[<Test>]
let modAdd () =
    Assert.AreEqual(6, modAdd 3 3 10)
    Assert.AreEqual(5, modAdd 8 7 10)
    
    Assert.AreEqual(8000000, modAdd 4000000 4000000 10000000)
    Assert.AreEqual(8000000, modAdd 9000000 9000000 10000000)
        
    Assert.AreEqual(8000000, modAdd 4000000 4000000 10000000)
    Assert.AreEqual(8000000, modAdd 9000000 9000000 10000000)
    
    Assert.AreEqual(9975308642L, modAdd 9987654321L 9987654321L 10000000000L)
    
    Assert.AreEqual(3709551614UL, modAdd 9223372036854775807UL 9223372036854775807UL 10000000000UL)

[<Test>]
let modMul () =
    Assert.AreEqual(9, modMul 3 3 10)
    Assert.AreEqual(5, modMul 3 5 10)

    Assert.AreEqual(8000000, modMul 2 4000000 10000000)
    Assert.AreEqual(8000000, modMul 2 9000000 10000000)
    
    Assert.AreEqual(8000000, modMul 2 4000000 10000000)
    Assert.AreEqual(8000000, modMul 2 9000000 10000000)

    Assert.AreEqual(1972825789, modMul 1234567899L 1111111L 10000000000L)
    Assert.AreEqual(15241579L, modMul 1234567899L 987654321L 10000000000L)

    Assert.AreEqual(9999990001UL, modMul 9999999999UL 9999UL 10000000000UL)
    Assert.AreEqual(3709551614UL, modMul 9223372036854775807UL 2UL 10000000000UL)

