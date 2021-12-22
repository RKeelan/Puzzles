module Euler25

(*
The Fibonacci sequence is defined by the recurrence relation:

Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
Hence the first 12 terms will be:

F1 = 1
F2 = 1
F3 = 2
F4 = 3
F5 = 5
F6 = 8
F7 = 13
F8 = 21
F9 = 34
F10 = 55
F11 = 89
F12 = 144
The 12th term, F12, is the first term to contain three digits.

What is the index of the first term in the Fibonacci sequence to contain 1000 digits?
*)

open BigInt
open Humanizer

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let mutable f1 = new BigInt(1L, BigInt.INT_64_RADIX)
    let mutable f2 = new BigInt(1L, BigInt.INT_64_RADIX) // This is the twelvth term
    let mutable fn = f1 + f2
    let mutable n = 3
    let d = 1000
    while (fn.ToString().Length < d) do
        f1 <- f2
        f2 <- fn
        fn <- f1 + f2
        n <- (n + 1)
        //if (Util.isDivisible n 10) then do printfn $"The {n.Ordinalize()} term of the Fibonacci sequence is {fn}"
    printfn $"The {n.Ordinalize()} term of the Fibonacci sequence is the first witn {d} digits"
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0