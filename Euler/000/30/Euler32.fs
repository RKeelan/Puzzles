module Euler32

(*
We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly
once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier,
and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1
through 9 pandigital.

HINT: Some products can be obtained in more than one way so be sure to only include it once in your
sum.
https://projecteuler.net/problem=32
*)

(*
1. list out every 4-digit number containing no duplicted digits
    - There's no way to make a 5 digit number (of greater) by multiplying the remaiing digits
        - 99*99 = 9801 and 999*9 = 8991
    - Similarly, there is no way to make a 3 digit number or smaller
2. For each of those numbers...
    a) I could either list factors first, then remove those that do not yield a pandigital number
    b) Or I could list out the numbers I can from the remaining digits, and see which ones can be
    multiplied to yield the product I need
    - I feel I feel like (a) will yield far fewer numbers, so that's the approach I'll take
3. Keep any number with more than one product pair
*)  

open System

let isPandigital (s:string) = (not (Util.hasDuplicates s)) && (s.Length = 9)

let isPandigitalProduct (n:int) =
    let factors = Util.factors n |> Seq.filter (fun (a,b) -> not ($"{a}{b}".Contains('0')))
    let panDigitalFactors = factors |> Seq.filter (fun (a,b) -> isPandigital $"{n}{a}{b}")
    if not (Seq.isEmpty panDigitalFactors) then do
        printfn $"{n}"
        printfn "\t%s" (String.Join("\n\t", panDigitalFactors))
    not (Seq.isEmpty panDigitalFactors)
    

let panDigitalCandidates =
    seq {1234..9876}
    |> Seq.filter (fun n -> not (n.ToString().Contains('0')))
    |> Seq.filter (fun n -> not (Util.hasDuplicates (n.ToString())))
    |> Seq.filter isPandigitalProduct


//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let sum = panDigitalCandidates |> Seq.sum
    printfn $"{sum}"
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0