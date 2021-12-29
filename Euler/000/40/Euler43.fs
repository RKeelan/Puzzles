module Euler43

(*
The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits
0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.

Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:

d2d3d4 = 406 is divisible by 2
d3d4d5 = 063 is divisible by 3
d4d5d6 = 635 is divisible by 5
d5d6d7 = 357 is divisible by 7
d6d7d8 = 572 is divisible by 11
d7d8d9 = 728 is divisible by 13
d8d9d10 = 289 is divisible by 17
Find the sum of all 0 to 9 pandigital numbers with this property.
https://projecteuler.net/problem=43
*)

(*
- There are 3,628,800 0 to 9 pandigital numbers, which is a lot, so I think it's worth doing a
prefiltering step
    - I can check the divisible by 2 and 5 criteria by checking if the relevant final digits are
    [0,2,4,6,8] or [0,5], respectively
    - Then I can perform the remaining checks using isDivisible
- I did try the pre-filtering route, but sadly it didn't save me any time and it also produced the
wrong answer
*)

open System
open Numbers
open Strings

let isSubstringDivisible (s:string) =
    // RK 29-Dec-2021: Not that Project Euler uses 1-indexing in the problem statement
    let d2_4 = Int32.Parse(s.Substring(1,3))    // Divisible by 2
    let d3_5 = Int32.Parse(s.Substring(2,3))    // Divisible by 3
    let d4_6 = Int32.Parse(s.Substring(3,3))    // Divisible by 5
    let d5_7 = Int32.Parse(s.Substring(4,3))    // Divisible by 7
    let d6_8 = Int32.Parse(s.Substring(5,3))    // Divisible by 11
    let d7_9 = Int32.Parse(s.Substring(6,3))    // Divisible by 13
    let d8_10 = Int32.Parse(s.Substring(7,3))   // Divisible by 17

    (isDivisible d2_4 2) && (isDivisible d4_6 5) && (isDivisible d3_5 3) && (isDivisible d5_7 7) &&
    (isDivisible d6_8 11) && (isDivisible d7_9 13) && (isDivisible d8_10 17)

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    let s = "1406357289"
    printfn $"1406357289 is substring divisible: {isSubstringDivisible s}"
    
    let pandigitalStrings = lexicographicPermutations "0123456789"
    let result = pandigitalStrings |> Seq.filter isSubstringDivisible |> Seq.map Int64.Parse
    let sum = result |> Seq.sum
    printfn $"The sum is {sum}"
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0