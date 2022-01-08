module Puzzles.Tests.FSharp.BigIntTests

open NUnit.Framework
open BigInt

let rec double (number : BigInt) (n : int64) =
    let numberDoubled = number * 2L
    match n with
    | 1L -> numberDoubled
    | _ -> double numberDoubled (n - 1L)

[<Test>]
let ctor () =
    let mutable bigInt = BigInt(0L, 10L)
    Assert.AreEqual("0", bigInt.ToString())

    bigInt <- BigInt(1L, 10L)
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
let comparison () =
    let mutable a = new BigInt(1,10)
    let mutable b = new BigInt(1,100)
    Assert.Throws<System.ArgumentException>(fun () -> (compare a b) |> ignore) |> ignore

    a <- new BigInt(1,10)
    b <- new BigInt(1,10)
    Assert.AreEqual(0, (compare a b))
    Assert.IsTrue(a >= b)
    Assert.IsTrue(a <= b)
    
    a <- new BigInt(2,10)
    b <- new BigInt(1,10)
    Assert.AreEqual(1, (compare a b))
    
    a <- new BigInt(1,10)
    b <- new BigInt(2,10)
    Assert.AreEqual(-1, (compare a b))
    
    a <- new BigInt(11,10)
    b <- new BigInt(9,10)
    Assert.AreEqual(1, (compare a b))
    
    a <- new BigInt(91,10)
    b <- new BigInt(19,10)
    Assert.AreEqual(1, (compare a b))

    Assert.IsTrue(a > b)
    Assert.IsTrue(a >= b)
    Assert.IsFalse(a < b)
    Assert.IsFalse(a <= b)

[<Test>]
let sumOfDigits () =
    let mutable bigInt = double (new BigInt(1L, BigInt.INT_32_RADIX)) 1000L
    Assert.AreEqual(1366, bigInt.sumOfDigits())

    bigInt <- BigInt.bigFactorial 100L
    Assert.AreEqual(648, bigInt.sumOfDigits())

[<Test>]
let scalar () =
    let mutable a = new BigInt(3L, 10L)
    Assert.AreEqual(Some(3L), (BigInt.scalar a))
    
    a <- new BigInt(33L, 10L)
    Assert.AreEqual(Some(33L), (BigInt.scalar a))
    
    a <- new BigInt(3L, 100L)
    Assert.AreEqual(Some(3L), (BigInt.scalar a))

    a <- double (new BigInt(1L, BigInt.INT_32_RADIX)) 1000L
    Assert.AreEqual(None, (BigInt.scalar a))

[<Test>]
let add () =
    let mutable a = new BigInt(6L, 10L)
    let mutable b = new BigInt(5L, 10L)
    let mutable c = a + b
    Assert.AreEqual("11", c.ToString())

    a <- new BigInt(21L, 10L)
    b <- new BigInt(103L, 10L)
    c <- a + b
    Assert.AreEqual("124", c.ToString())
    
    a <- new BigInt(123456789L, 10L)
    b <- new BigInt(9L, 10L)
    c <- a + b
    Assert.AreEqual("123456798", c.ToString())

[<Test>]
let subtractRadix10 () =
    // No Borrow
    let mutable a = new BigInt(6L, 10L)
    let mutable b = new BigInt(5L, 10L)
    let mutable expected = BigInt(1L, 10L)
    Assert.AreEqual(expected, a-b)

    a <- new BigInt(11L, 10L)
    b <- new BigInt(5L, 10L)
    expected <- BigInt(6L, 10L)
    Assert.AreEqual(expected, a-b)
    
    a <- new BigInt(10L, 10L)
    b <- new BigInt(9L, 10L)
    expected <- BigInt(1L, 10L)
    Assert.AreEqual(expected, a-b)
    
    a <- new BigInt(100L, 10L)
    b <- new BigInt(99L, 10L)
    expected <- BigInt(1L, 10L)
    Assert.AreEqual(expected, a-b)
    
    a <- new BigInt(100000L, 10L)
    b <- new BigInt(99999L, 10L)
    expected <- BigInt(1L, 10L)
    Assert.AreEqual(expected, a-b)

    Assert.Throws<System.ArgumentException>(fun () -> (b-a) |> ignore) |> ignore
    
[<Test>]
let subtractRadix100 () =
    // No Borrow
    let mutable a = new BigInt(90L, 100L)
    let mutable b = new BigInt(55L, 100L)
    let mutable expected = BigInt(35L, 100L)
    Assert.AreEqual(expected, a-b)
    
    a <- new BigInt(1082L, 100L)
    b <- new BigInt(83L, 100L)
    expected <- BigInt(999L, 100L)
    Assert.AreEqual(expected, a-b)
        
    // Borrow in first digit
    a <- new BigInt(100000L, 100L)
    b <- new BigInt(99999L, 100L)
    expected <- BigInt(1L, 100L)
    Assert.AreEqual(expected, a-b)
    
    Assert.Throws<System.ArgumentException>(fun () -> (b-a) |> ignore) |> ignore
    
[<Test>]
let subtractRadixInt32Max () =
    let mutable a = new BigInt(4379823623L, BigInt.INT_32_RADIX)
    let mutable b = new BigInt(3879482793L, BigInt.INT_32_RADIX)
    let mutable expected = BigInt(500340830L, BigInt.INT_32_RADIX)
    Assert.AreEqual(expected, a-b)
    
    a <- new BigInt(1532938268050L, BigInt.INT_32_RADIX)
    b <- new BigInt(104746035411L, BigInt.INT_32_RADIX)
    expected <- BigInt(1428192232639L, BigInt.INT_32_RADIX)
    Assert.AreEqual(expected, a-b)
    
    Assert.Throws<System.ArgumentException>(fun () -> (b-a) |> ignore) |> ignore
    
[<Test>]
let subtractRadixInt64Max () =
    let mutable a = new BigInt(10949559058L, BigInt.INT_64_RADIX)
    let mutable b = new BigInt(6595120748L, BigInt.INT_64_RADIX)
    let mutable expected = BigInt(4354438310L, BigInt.INT_64_RADIX)
    Assert.AreEqual(expected, a-b)
    
    a <- new BigInt(2299407402075L, BigInt.INT_64_RADIX)
    b <- new BigInt(1361698460343L, BigInt.INT_64_RADIX)
    expected <- BigInt(937708941732L, BigInt.INT_64_RADIX)
    Assert.AreEqual(expected, a-b)

    a <- a * 255L
    b <- b * 255L
    expected <- BigInt(239115780141660L, BigInt.INT_64_RADIX)
    
    Assert.Throws<System.ArgumentException>(fun () -> (b-a) |> ignore) |> ignore

[<Test>]
let multiplyScalar () =
    let mutable bigInt = BigInt(6L, 10L)
    bigInt <- bigInt * 2L
    Assert.AreEqual("12", bigInt.ToString())
    
    let bigInt = double (new BigInt(1L, BigInt.INT_32_RADIX)) 1000L
    let bigResult = "107150860718626732094842504906000181056140481170553360744375038837035105112" +
                    "493612249319837881569585812759467291755314682518714528569231404359845775746" +
                    "985748039345677748242309854210746050623711418779541821530464749835819412673" +
                    "98767559165543946077062914571196477686542167660429831652624386837205668069376"
    Assert.AreEqual(bigResult, bigInt.ToString())

[<Test>]
let multiplyBigInt () =
    let mutable a = BigInt(3L, 10L)
    let mutable b = BigInt(2L, 10L)
    let mutable expected = BigInt(6L, 10L)
    Assert.AreEqual(expected, a*b)
    
    let mutable a = BigInt(6L, 10L)
    let mutable b = BigInt(2L, 10L)
    let mutable expected = BigInt(12L, 10L)
    Assert.AreEqual(expected, a*b)
    
    let mutable a = BigInt(12L, 10L)
    let mutable b = BigInt(12L, 10L)
    let mutable expected = BigInt(144L, 10L)
    Assert.AreEqual(expected, a*b)
    
    let mutable a = BigInt(8000000L, BigInt.INT_32_RADIX)
    let mutable b = BigInt(8000000L, BigInt.INT_32_RADIX)
    let mutable expected = BigInt(64000000000000L, BigInt.INT_32_RADIX)
    Assert.AreEqual(expected, a*b)

[<Test>]
let division () =
    let mutable a = BigInt(5L, 10L)
    let mutable b = BigInt(1L, 10L)
    let mutable expectedQuotient = BigInt(5L,10L)
    let mutable expectedRemainder = BigInt(0L,10L)
    let mutable (quotient, remainder) = a/b
    Assert.AreEqual(expectedQuotient, quotient)
    Assert.AreEqual(expectedRemainder, remainder)
    
    b <- BigInt(2L, 10L)
    let mutable expectedQuotient = BigInt(2L,10L)
    let mutable expectedRemainder = BigInt(1L,10L)
    let mutable (quotient, remainder) = a/b
    Assert.AreEqual(expectedQuotient, quotient)
    Assert.AreEqual(expectedRemainder, remainder)
    
    b <- BigInt(3L, 10L)
    let mutable expectedQuotient = BigInt(1L,10L)
    let mutable expectedRemainder = BigInt(2L,10L)
    let mutable (quotient, remainder) = a/b
    Assert.AreEqual(expectedQuotient, quotient)
    Assert.AreEqual(expectedRemainder, remainder)
    
    b <- BigInt(6L, 10L)
    let mutable expectedQuotient = BigInt(0L,10L)
    let mutable expectedRemainder = BigInt(5L,10L)
    let mutable (quotient, remainder) = a/b
    Assert.AreEqual(expectedQuotient, quotient)
    Assert.AreEqual(expectedRemainder, remainder)
    
    a <- BigInt(55L, 10L)
    b <- BigInt(5L, 10L)
    let mutable expectedQuotient = BigInt(11L,10L)
    let mutable expectedRemainder = BigInt(0L,10L)
    let mutable (quotient, remainder) = a/b
    Assert.AreEqual(expectedQuotient, quotient)
    Assert.AreEqual(expectedRemainder, remainder)
    
    a <- BigInt(55L, 10L)
    b <- BigInt(11L, 10L)
    let mutable expectedQuotient = BigInt(5L,10L)
    let mutable expectedRemainder = BigInt(0L,10L)
    let mutable (quotient, remainder) = a/b
    Assert.AreEqual(expectedQuotient, quotient)
    Assert.AreEqual(expectedRemainder, remainder)
    
    a <- BigInt(55L, 10L)
    b <- BigInt(10L, 10L)
    let mutable expectedQuotient = BigInt(5L,10L)
    let mutable expectedRemainder = BigInt(5L,10L)
    let mutable (quotient, remainder) = a/b
    Assert.AreEqual(expectedQuotient, quotient)
    Assert.AreEqual(expectedRemainder, remainder)

    a <- BigInt(5L, 10L)
    b <- BigInt(5L, 100L)
    Assert.Throws<System.ArgumentException>(fun () -> (b-a) |> ignore) |> ignore

    b <- BigInt.zero 10L
    Assert.Throws<System.DivideByZeroException>(fun () -> (a/b) |> ignore) |> ignore