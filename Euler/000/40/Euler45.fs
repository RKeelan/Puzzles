﻿module Euler45

(*
Triangle, pentagonal, and hexagonal numbers are generated by the following formulae:

Triangle	 	Tn=n(n+1)/2	 	1, 3, 6, 10, 15, ...
Pentagonal	 	Pn=n(3n−1)/2	 	1, 5, 12, 22, 35, ...
Hexagonal	 	Hn=n(2n−1)	 	1, 6, 15, 28, 45, ...
It can be verified that T285 = P165 = H143 = 40755.

Find the next triangle number that is also pentagonal and hexagonal.
https://projecteuler.net/problem=45
*)

(*
- One approach is to sequentially examine every traingle number from T185 up until I find one that
is also a pentagon number and a hexagon number
*)

open System
open Numbers

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    // Code goes here
    let start = 144

    // RK 30-Dec-2021: I aborted this approach after a minute of run-time
    //let triangleNums = Seq.initInfinite (fun n -> triangleNumber (n+start))
    //let triPentaHeaxgonNumber = triangleNums |> Seq.find (fun n -> (isPentagonNumber n) && (isHexagonNumber n))
    //printfn $"The first triangle number after {start} that is also a pentagon and hexgaon number is {triPentaHeaxgonNumber}"
    
    // RK 30-Dec-2021: I tried this next, and it ran in 130 ms.
    // The reason it's faster is that each new hexagon number is a bigger jump, so you get to the
    // end faster, but even still, I'm taken aback by how much difference this made
    let hexNums = Seq.initInfinite (fun n -> hexagonNumber (n+start))
    let triPentaHeaxgonNumber = hexNums |> Seq.find (fun n -> (isTriangleNumber n) && (isPentagonNumber n))
    printfn $"The first triangle number after {start} that is also a pentagon and hexgaon number is {triPentaHeaxgonNumber}"
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0