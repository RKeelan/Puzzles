module Euler56

(*
A googol (10100) is a massive number: one followed by one-hundred zeros; 100100 is almost
unimaginably large: one followed by two-hundred zeros. Despite their size, the sum of the digits
in each number is only 1.

Considering natural numbers of the form, ab, where a, b < 100, what is the maximum digital sum?
https://projecteuler.net/problem=56
*)

open System
open BigInt

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let N = 99
    let maxDigitalSum =
        seq {
        for a in [1..N] do
            for b in [1..N] do
                yield (new BigInt(a))**b

        }
        |> Seq.map (fun b -> b.sumOfDigits())
        |> Seq.max
    printfn $"The maximum digital sum of powers below {N+1} is {maxDigitalSum}"
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0