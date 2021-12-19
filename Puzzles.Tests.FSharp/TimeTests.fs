module Puzzles.Tests.FSharp.TimeTests

open Time
open NUnit.Framework

[<Test>]
let isLeapYear () =
    Assert.IsFalse(Time.isLeapYear 1599)
    Assert.IsTrue(Time.isLeapYear 1600)
    Assert.IsFalse(Time.isLeapYear 1601)
    Assert.IsFalse(Time.isLeapYear 1603)
    Assert.IsTrue(Time.isLeapYear 1604)
    Assert.IsFalse(Time.isLeapYear 1605)
    
    Assert.IsTrue(Time.isLeapYear 1896)
    Assert.IsFalse(Time.isLeapYear 1897)
    Assert.IsFalse(Time.isLeapYear 1898)
    Assert.IsFalse(Time.isLeapYear 1899)
    Assert.IsFalse(Time.isLeapYear 1900)
    Assert.IsFalse(Time.isLeapYear 1901)
    Assert.IsFalse(Time.isLeapYear 1902)
    Assert.IsFalse(Time.isLeapYear 1903)
    Assert.IsTrue(Time.isLeapYear 1904)

[<Test>]
let daysInYear () =
    Assert.AreEqual(365, Time.daysInYear 1599)
    Assert.AreEqual(366, Time.daysInYear 1600)
    Assert.AreEqual(365, Time.daysInYear 1601)
    Assert.AreEqual(365, Time.daysInYear 1603)
    Assert.AreEqual(366, Time.daysInYear 1604)
    Assert.AreEqual(365, Time.daysInYear 1605)
    
    Assert.AreEqual(366, Time.daysInYear 1896)
    Assert.AreEqual(365, Time.daysInYear 1897)
    Assert.AreEqual(365, Time.daysInYear 1898)
    Assert.AreEqual(365, Time.daysInYear 1899)
    Assert.AreEqual(365, Time.daysInYear 1900)
    Assert.AreEqual(365, Time.daysInYear 1901)
    Assert.AreEqual(365, Time.daysInYear 1902)
    Assert.AreEqual(365, Time.daysInYear 1903)
    Assert.AreEqual(366, Time.daysInYear 1904)