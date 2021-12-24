module Euler10

(*
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
*)

open System.Linq

//[<EntryPoint>]
let main argv =
    let primesBelow10 = Prime.rkPrimes.TakeWhile(fun p -> p < 10)
    let sumOfPrimesBelow10 = Seq.sum primesBelow10
    printfn $"The sum of primes below 10 is {sumOfPrimesBelow10}"
    
    // This took about a minute to run. Euler.net says that the problems should be solveable within
    // 1 minute of runtime on a modestly powered computer (circa 2001!), so arguably my algorithm
    // here isn't good enough.
    //
    // On the other hand, I spent a ton of extra time on this because Euler.net doesn't handle
    // commas properly, so I'll call it a draw
    //let primesBelow2000000 = Prime.rkPrimes.TakeWhile(fun p -> p < 2000000) |> Seq.map int64
    //let sumOfPrimesBelow2000000 : int64 = Seq.sum primesBelow2000000
    //let largestPrime : int64 = primesBelow2000000.Last()
    //printfn $"The largest prime is {largestPrime:N0}"
    //printf $"The sum of primes below 2,000,000 is {sumOfPrimesBelow2000000}"

    // RK Using a prime sieve from Stack Overflow, I can do the same problem in 3s. Since I'm more
    // interested in learning F# then math, and I've done a ton of dicking around with Prime sieves
    // already, I'll use the SO algorithm where required
    let primesBelow2000000 = Prime.soPrimes().TakeWhile(fun p -> p < 2000000L)
    let sumOfPrimesBelow2000000 : int64 = Seq.sum primesBelow2000000
    let largestPrime : int64 = primesBelow2000000.Last()
    printfn $"The largest prime is {largestPrime:N0}"
    printf $"The sum of primes below 2,000,000 is {sumOfPrimesBelow2000000}"

    // RK This is an imperative algorithm I cooked up while I was trying to figure out why my
    // functional version above wasn't working (turns out Euler.net doesn't handle commas!)
    // It took about 1m30s to run.
    //let limit = 20
    //let mutable sum : int64 = 0L;
    //let mutable p = Prime.nextPrime 0;
    //let mutable i = 0;
    //while (p < limit) do
    //    i <- i + 1
    //    printfn $"The sum of primes up to {(p-1):N0} is {sum:N0}"
    //    sum <- sum + int64(p)
    //    p <- Prime.nextPrime p
    //printfn $"The sum of primes up to {p:N0} is {sum:N0}"
    0
    