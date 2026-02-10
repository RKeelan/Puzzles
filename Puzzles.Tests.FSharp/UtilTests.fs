module Puzzles.Tests.FSharp.UtilTests

open System
open NUnit.Framework
open NUnit.Framework.Legacy
open Util

[<Test>]
let uncheckedAddition () =
    // Signed Int32
    ClassicAssert.AreEqual(2, (1 +! 1))
    ClassicAssert.AreEqual(0x80000000, (1 +! System.Int32.MaxValue))
    
    // Signed Int64
    ClassicAssert.AreEqual(2L, (1L +! 1L))
    ClassicAssert.AreEqual(0x80000000L, (1L +! int64(System.Int32.MaxValue)))
    ClassicAssert.AreEqual(0x8000000000000000L, (1L +! System.Int64.MaxValue))

    // Unsigned Int32
    ClassicAssert.AreEqual(2u, (1u +! 1u))
    ClassicAssert.AreEqual(0u, (1u +! System.UInt32.MaxValue))
    
    // Unsigned Int64
    ClassicAssert.AreEqual(2UL, (1UL +! 1UL))
    ClassicAssert.AreEqual(0x100000000UL, (1UL +! uint64(System.UInt32.MaxValue)))
    ClassicAssert.AreEqual(0UL, (1UL +! System.UInt64.MaxValue))

[<Test>]
let uncheckedMultiplication () =
    // Signed Int32
    ClassicAssert.AreEqual(4, (2 *! 2))
    ClassicAssert.AreEqual(0xFFFFFFFE, (2 *! System.Int32.MaxValue))
    
    // Signed Int64
    ClassicAssert.AreEqual(16L, (4L *! 4L))
    ClassicAssert.AreEqual(0xFFFFFFFFFFFFFFFCL, (4L *! System.Int64.MaxValue))

    // Unsigned Int32
    ClassicAssert.AreEqual(9u, (3u *! 3u))
    ClassicAssert.AreEqual(4294967293u, (3u *! System.UInt32.MaxValue))
    
    // Unsigned Int64
    ClassicAssert.AreEqual(25UL, (5UL *! 5UL))
    ClassicAssert.AreEqual(18446744073709551611UL, (5UL *! System.UInt64.MaxValue))
    
[<Test>]
let modAdd () =
    ClassicAssert.AreEqual(6, modAdd 3 3 10)
    ClassicAssert.AreEqual(5, modAdd 8 7 10)
    
    ClassicAssert.AreEqual(8000000, modAdd 4000000 4000000 10000000)
    ClassicAssert.AreEqual(8000000, modAdd 9000000 9000000 10000000)
        
    ClassicAssert.AreEqual(8000000, modAdd 4000000 4000000 10000000)
    ClassicAssert.AreEqual(8000000, modAdd 9000000 9000000 10000000)
    
    ClassicAssert.AreEqual(9975308642L, modAdd 9987654321L 9987654321L 10000000000L)
    
    ClassicAssert.AreEqual(3709551614UL, modAdd 9223372036854775807UL 9223372036854775807UL 10000000000UL)

[<Test>]
let modMul () =
    ClassicAssert.AreEqual(9, modMul 3 3 10)
    ClassicAssert.AreEqual(5, modMul 3 5 10)

    ClassicAssert.AreEqual(8000000, modMul 2 4000000 10000000)
    ClassicAssert.AreEqual(8000000, modMul 2 9000000 10000000)
    
    ClassicAssert.AreEqual(8000000, modMul 2 4000000 10000000)
    ClassicAssert.AreEqual(8000000, modMul 2 9000000 10000000)

    ClassicAssert.AreEqual(1972825789, modMul 1234567899L 1111111L 10000000000L)
    ClassicAssert.AreEqual(15241579L, modMul 1234567899L 987654321L 10000000000L)

    ClassicAssert.AreEqual(9999990001UL, modMul 9999999999UL 9999UL 10000000000UL)
    ClassicAssert.AreEqual(3709551614UL, modMul 9223372036854775807UL 2UL 10000000000UL)