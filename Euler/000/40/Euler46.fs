module Euler46

(*
It was proposed by Christian Goldbach that every odd composite number can be written as the sum of
a prime and twice a square.

9 = 7 + 2×12
15 = 7 + 2×22
21 = 3 + 2×32
25 = 7 + 2×32
27 = 19 + 2×22
33 = 31 + 2×12

It turns out that the conjecture was false.

What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
https://projecteuler.net/problem=46
*)

open System
open Numbers
open Primes

// Goldbach has a famous, unproven conjecture thateven even whole number greater than 2 is the sum
// of two prime numbers.
// He also had a much less famous, false conjecture. The false conjecture is the subject of this
// function
let goldbackExample n =
    let primes = naiveSieve n
    if List.last primes = n then true // Because n was prime
    else
        primes |> List.exists (fun p -> 
            let residual = double (n - p)
            isInteger (sqrt(residual/2.))
        )

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    printfn $"9 is a positive Goldbach example: {goldbackExample 9}"
    printfn $"11 is a positive Goldbach example: {goldbackExample 11}"
    printfn $"15 is a positive Goldbach example: {goldbackExample 15}"
    printfn $"21 is a positive Goldbach example: {goldbackExample 21}"
    printfn $"25 is a positive Goldbach example: {goldbackExample 25}"
    printfn $"27 is a positive Goldbach example: {goldbackExample 27}"
    printfn $"33 is a positive Goldbach example: {goldbackExample 33}"

    let oddNumbers = Seq.initInfinite (fun n -> (n*2+3))
    let goldbachCounterExample = oddNumbers |> Seq.find (fun n -> not(goldbackExample n))
    printfn $"The first Godlback counter-example is {goldbachCounterExample}"
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0