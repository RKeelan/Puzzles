module Euler35

(*
The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719,
are themselves prime.

There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

How many circular primes are there below one million?
https://projecteuler.net/problem=35
*)

open System
open Strings

let isCircularPrime n =
    let rotations = rotations (n.ToString()) |> Seq.toList |> Seq.map Int32.Parse |> Seq.toList
    let primeRotations = rotations |> List.filter Primes.isPrime
    rotations.Length = primeRotations.Length

let circularPrimes n =
    seq {2..n}
    |> Seq.filter isCircularPrime
    |> Seq.distinct

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let rotations197 = rotations "197"
    printfn "%s" (String.Join('\n', rotations197))
    printfn $"197 is a circular prime: {isCircularPrime 197}"
    printfn $"971 is a circular prime: {isCircularPrime 971}"
    printfn $"719 is a circular prime: {isCircularPrime 719}"

    let N = 999999
    let circularPrimes = circularPrimes N |> Seq.toList
    printfn $"There are {circularPrimes.Length} circular primes below {(N+1):N0}:"
    printfn "%s" (String.Join('\n', circularPrimes))

    // RK 27-Dec-2021: This took 104s to run, which is well over spec. I think the first
    // optimization I'd think about making is to somehow remove the rotations from my list of
    // numbers to check. (E.g., once I've shown that 197 is a circular prime, I don't need to check
    // 971 and 719
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0