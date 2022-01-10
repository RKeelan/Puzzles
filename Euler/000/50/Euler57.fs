module Euler57

(*
It is possible to show that the square root of two can be expressed as an infinite continued
fraction.

(There's a bunch of Latex-style formulas here which I don't care to convert to ascii)

In the first one-thousand expansions, how many fractions contain a numerator with more digits than
the denominator?
https://projecteuler.net/problem=57
*)

(*
- Denominator n = Denominator (n-1) + Numberator (n-1)
- Numerator n = Denominator n + Denominator (n-1)
*)

open System
open Numbers
open BigInt

let nextExpansion (n:BigInt,d:BigInt) : (BigInt*BigInt) =
    let nextD = n+d
    let nextN = nextD+d
    (nextN,nextD)

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let N = 999
    let first = (new BigInt(3L), new BigInt(2L))
    let expansions = first |> Seq.unfold (fun (n,d) -> Some((n,d), nextExpansion(n,d))) |> Seq.take N
    //expansions |> Seq.iter (fun (n,d) -> printfn $"{n}/{d} = {(double n)/(double d)}")
    let count = expansions |> Seq.filter (fun (n,d) -> (numDigits n) > (numDigits d)) |> Seq.length
    printfn $"{count} fractions within the first {N} expansions have longer numerators than denominators."
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0