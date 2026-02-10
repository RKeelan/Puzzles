module Puzzles.Tests.FSharp.TimeTests

open Time
open NUnit.Framework
open NUnit.Framework.Legacy

[<Test>]
let isLeapYear () =
    ClassicAssert.IsFalse(Time.isLeapYear 1599)
    ClassicAssert.IsTrue(Time.isLeapYear 1600)
    ClassicAssert.IsFalse(Time.isLeapYear 1601)
    ClassicAssert.IsFalse(Time.isLeapYear 1603)
    ClassicAssert.IsTrue(Time.isLeapYear 1604)
    ClassicAssert.IsFalse(Time.isLeapYear 1605)
    
    ClassicAssert.IsTrue(Time.isLeapYear 1896)
    ClassicAssert.IsFalse(Time.isLeapYear 1897)
    ClassicAssert.IsFalse(Time.isLeapYear 1898)
    ClassicAssert.IsFalse(Time.isLeapYear 1899)
    ClassicAssert.IsFalse(Time.isLeapYear 1900)
    ClassicAssert.IsFalse(Time.isLeapYear 1901)
    ClassicAssert.IsFalse(Time.isLeapYear 1902)
    ClassicAssert.IsFalse(Time.isLeapYear 1903)
    ClassicAssert.IsTrue(Time.isLeapYear 1904)

[<Test>]
let daysInYear () =
    ClassicAssert.AreEqual(365, Time.daysInYear 1599)
    ClassicAssert.AreEqual(366, Time.daysInYear 1600)
    ClassicAssert.AreEqual(365, Time.daysInYear 1601)
    ClassicAssert.AreEqual(365, Time.daysInYear 1603)
    ClassicAssert.AreEqual(366, Time.daysInYear 1604)
    ClassicAssert.AreEqual(365, Time.daysInYear 1605)
    
    ClassicAssert.AreEqual(366, Time.daysInYear 1896)
    ClassicAssert.AreEqual(365, Time.daysInYear 1897)
    ClassicAssert.AreEqual(365, Time.daysInYear 1898)
    ClassicAssert.AreEqual(365, Time.daysInYear 1899)
    ClassicAssert.AreEqual(365, Time.daysInYear 1900)
    ClassicAssert.AreEqual(365, Time.daysInYear 1901)
    ClassicAssert.AreEqual(365, Time.daysInYear 1902)
    ClassicAssert.AreEqual(365, Time.daysInYear 1903)
    ClassicAssert.AreEqual(366, Time.daysInYear 1904)