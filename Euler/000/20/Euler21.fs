module Euler21

(*
Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.
https://projecteuler.net/problem=21
*)

open Numbers

let areAmicable a b =
    let sumOfDivisorsA = divisorsExSelf a |> Seq.sum
    let sumOfDivisorsB = divisorsExSelf b |> Seq.sum
    //printfn $"Sum of {a}'s divisors is {sumOfDivisorsA}"
    //printfn $"Sum of {b}'s divisors is {sumOfDivisorsB}"
    (a = sumOfDivisorsB) && (b = sumOfDivisorsA)

let amicablePair n =
    let candidate = (divisorsExSelf n) |> Seq.sum
    //printfn $"\tChecking {n} and {candidate}"
    if n = candidate then None
    elif areAmicable n candidate then Some(candidate)
    else None

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    //let a = 6
    //let b = 28
    //printfn "%d's divisors: %s" a (String.Join(",", (amicableDivisors a)))
    //printfn $"{a}'s amicable pair: {amicablePair a}"

    let N = 10000
    let amicableNumbers = seq { for i in 1 .. (N-1) -> amicablePair i} |> Seq.choose id
    printfn $"The amicable numbers under {N:N0}"
    amicableNumbers |> Seq.iter (fun n -> printfn $"{n}")
    printfn $"The sum of all amicable numbers under {N:N0} is {amicableNumbers |> Seq.sum}"
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0