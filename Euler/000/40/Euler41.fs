module Euler41

(*
We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.

What is the largest n-digit pandigital prime that exists?
https://projecteuler.net/problem=41
*)

(*
- My first attempt at this naively used a prime sieve, then filtered for primes that were pandigital
- I aborted that after 1.5 minutes of runtime to try an approach using permutations of the digits [1..9]
- I just assumed that the largest n-digit pandigital prime would be 9 digits, but maybe not
*)
                                                                                                   
open System

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    // Lets try the naive solution
    //let max = Prime.naivePrimeSieve 987654322 |> Seq.filter (fun n -> Util.isPandigital (n.ToString()) 9) |> Seq.max
    //printfn $"The largest pandigital prime is {max}"
    // RK 28-Dec-2021: The naive solution sucked

    // RK 28-Dec-2021: I cheated a bit here, and ran the code with "123456789" first. When that ran
    // for a full minute, I aborted and tried "12345678"
    let digits = "1234567"
    let numbers = Util.lexicographicPermutations digits |> Seq.map Int64.Parse
    let candidates = numbers |> Seq.filter (fun n -> not (Util.isEven64 n)) |> Seq.toList |> List.sortByDescending id
    //let odd = List.head candidates
    //printfn $"The largest odd pandigital number is {odd}"
    let primes = candidates |> Seq.filter Prime.isPrime64
    let largest = Seq.head primes

    //let primes = numbers |> Seq.filter (fun n -> not (Util.isEven64 n)) |> Seq.filter Prime.isPrime64
    //let largest = primes |> Seq.max
    printfn $"The largest {digits.Length}-digit prime is {largest}"
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0