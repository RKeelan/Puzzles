module Euler20

(*
n! means n × (n − 1) × ... × 3 × 2 × 1

For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100!
https://projecteuler.net/problem=20
*)

open BigInt

//[<EntryPoint>]
let main argv =
    let n = 10L
    let test = BigInt.bigFactorial n
    printfn $"{n}! is {test}"
    printfn $"The sum of the digits of {n}! is {test.sumOfDigits()}"
    
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    let N = 100L
    let bigInt = BigInt.bigFactorial N
    printfn $"{N}! is {bigInt}"
    printfn $"The sum of the digits of {N}! is {bigInt.sumOfDigits()}"

    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0