module Euler19

(*
You are given the following information, but you may prefer to do some research for yourself.

- 1 Jan 1900 was a Monday.
- Thirty days has September,
  April, June and November.
  All the rest have thirty-one,
  Saving February alone,
  Which has twenty-eight, rain or shine.
  And on leap years, twenty-nine.
- A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
https://projecteuler.net/problem=19
*)

(*
- How to approach this?
    - I can make a function that returns the day of the year for a particular month
        - E.g., First of January is always day 1
    - Then I need a function that calcultes day of week for a given year and day
        - Or maybe just calculates days since 07-Jan-1900 (the first sunday of 1900)?
            - If days since 07-Jan-1900 mod 7 = 0, then the day is a sunday
*)

open System
open Time
open Numbers

[<Literal>]
let EPOC_DAY = 7
let EPOC_YEAR = 1900

let daysSinceEpoc dayOfYear year =
    if year < EPOC_YEAR then do
        raise (ArgumentException $"{year} is before the epoc ({EPOC_YEAR})")
    if year = EPOC_YEAR && dayOfYear < EPOC_DAY then do
        raise (ArgumentException $"Day {dayOfYear} of {year} is before the epoc ({EPOC_DAY} of {EPOC_YEAR})")

    let daysBetweenEpocAndStartOfYear =
        seq {for y = EPOC_YEAR to year-1 do Time.daysInYear y} |> Seq.fold (fun acc days -> acc + days) 0
    dayOfYear + daysBetweenEpocAndStartOfYear - EPOC_DAY

let isSunday day year =
    let days = daysSinceEpoc day year
    isDivisible days 7
     
//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    let day = 6
    let year = 1901
    printfn $"{daysSinceEpoc day year}"
    printfn $"The {day}th day of {year} was a Sunday: {isSunday day year}"
    
    let firstOfMonths = seq {
        for y in 1901 .. 2000 do
            for m in Time.Months ->
                (m,y)
    }
    //firstOfMonths |> Seq.iter (fun (month, year) -> printfn $"The start of {month} was {firstOfMonth month year} of {year}")
    let firstSundays = firstOfMonths |> Seq.filter (fun (month, year) -> isSunday (firstOfMonth month year) year) |> Seq.toList
    firstSundays |> Seq.iter (fun (month, year) -> printfn $"{month}-{year}")
    printfn $"{firstSundays.Length} Sundays fell on the first of the month during the 20th century."
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0