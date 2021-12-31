module Euler47

(*
The first two consecutive numbers to have two distinct prime factors are:

14 = 2 × 7
15 = 3 × 5

The first three consecutive numbers to have three distinct prime factors are:

644 = 2² × 7 × 23
645 = 3 × 5 × 43
646 = 2 × 17 × 19.

Find the first four consecutive integers to have four distinct prime factors each. What is the
first of these numbers?
https://projecteuler.net/problem=47
*)

(*
- I think the thing about "distinct" is just that, for example, the two 2s in 644's prime
factorization are just one distinct factor (2)
*)

open System
open Primes

let distinctPrimeFactors n = factorize n |> List.distinct |> List.length

let criteria n =
    ((distinctPrimeFactors n) = 4) &&
    ((distinctPrimeFactors (n+1)) = 4) &&
    ((distinctPrimeFactors (n+2)) = 4) &&
    ((distinctPrimeFactors (n+3)) = 4)


//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let integer = Seq.initInfinite (fun n -> n+1) |> Seq.find criteria
    printfn $"The first of the four integers is {integer}"
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0