module Euler58

(*
Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length
7 is formed.

37 36 35 34 33 32 31
38 17 16 15 14 13 30
39 18  5  4  3 12 29
40 19  6  1  2 11 28
41 20  7  8  9 10 27
42 21 22 23 24 25 26
43 44 45 46 47 48 49

It is interesting to note that the odd squares lie along the bottom right diagonal, but what is
more interesting is that 8 out of the 13 numbers lying along both diagonals are prime; that is, a
ratio of 8/13 ≈ 62%.

If one complete new layer is wrapped around the spiral above, a square spiral with side length 9
will be formed. If this process is continued, what is the side length of the square spiral for
which the ratio of primes along both diagonals first falls below 10%?

https://projecteuler.net/problem=58
*)

(*
Basically, I need to find the diagonals of square-spiral with length N, then find out how many
are prime
- Make a memoized isPrime
    - cacheing the result of isPrime didn't help very much
- Instead, I'll run run the calculations for spiral N based on spiral N-1, and only check if the
    four new numbers are prime
*)

open System
open Primes

let primeFraction N =
    // Intuitively, each diagonal extends out from the center into either the top-left, top-right
    // (etc) corner. Since it's from the center but excluding the center, it's half of N-1
    let termsInADiagonal = (N-1)/2

    // The total length of the sequence is the number of terms in each diagonal * the number of
    // diagonals, plus one for the center
    let sequenceLength = termsInADiagonal*4 + 1
    let increments = Seq.init (sequenceLength - 1) (fun n -> 2*(1+(n/4)))
    //printfn "Increments: %s" (String.Join(" ", increments))
    let terms = Seq.scan (fun acc increment -> (acc + increment)) 1 increments |> Seq.toList
    let primeTerms = terms |> List.filter Primes.isPrime
    (double primeTerms.Length) / (double terms.Length)

[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let sideLengths = Seq.initInfinite (fun n -> 7+2*n)
    let result = sideLengths |> Seq.map (fun n -> primeFraction n) |> Seq.find (fun f -> f < 0.1)
    printfn $"The side length is {result}"

    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0