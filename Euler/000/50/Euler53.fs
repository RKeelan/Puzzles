module Euler53

(*
There are exactly ten ways of selecting three from five, 12345:

123, 124, 125, 134, 135, 145, 234, 235, 245, and 345

In combinatorics, we use the notation 5C3 = 10, 
.

In general,nCr = n!/(r!*(n-r)!), where r <= n, and 0! = 1.

It is not until n = 23, that a value exceeds one-million: 23C10 = 1,144,066

How many, not necessarily distinct, values of nCr for 1 <= n <= 100, are greater than 
one-million?
https://projecteuler.net/problem=53
*)

(*
- Okay, the trick is that n! and r! (in the nCr formula) can potentially be very large
- To make this work, I need to add multiplication and division to my BigInt class
- I tried this using the simplist division algorithm (repeated subtraction), and it was totally
unworkabled. Just 23C10 takes almost two seconds to compute, and this question requires calculating
4,950 combinations many of which will have far worse performance characteris
- From stack overflow:
    - https://stackoverflow.com/questions/11809502/which-is-better-way-to-calculate-ncr
    - C(n,r) = C(n-1,r) + (Cn-1,r-1)
    - The key valuable thing here is that once either of the summed terms equals 0, I can stop
    calculating
*)

open System
open System.Collections.Generic
open Numbers
open BigInt
open Combinatronics



let cache = Dictionary<(int*int),BigInt>()

let rec C n r =
    match cache.TryGetValue((n,r)) with
    | (true, x) -> x
    | (false, _) ->
        if n = r then new BigInt(1L)
        elif r = 1 then BigInt(n)
        else
            let result = (C (n - 1) r) + (C (n - 1) (r - 1))
            cache.Add((n,r),result)
            result

    

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    //let n = 23L
    //let r = 10L
    //let nF = BigInt.bigFactorial n
    //let rF = BigInt.bigFactorial r
    //let nrF = BigInt.bigFactorial (n-r)
    //let rFNrF = rF * nrF
    //let cF = BigInt.scalar (NCR n r)
    //printfn $"{n}! = {nF}"
    //printfn $"{r}! = {rF}"
    //printfn $"({n}-{r})! = {nrF}"
    //printfn $"{r}! * ({n}-{r})! = {rFNrF}"
    //printfn $"{n} choose {r} = {cF:N0}"

    //let n = 23
    //let r = 10
    //let c = (C n r)
    //printfn $"{n} choose {r} = {c}"

    let N = 100
    let LIMIT = 1000000
    let nCrs = seq {
        for n in 2..N do
            for r in 1..(n-1) do
                yield C n r
    }
    let result = nCrs |> Seq.filter (fun n -> n > new BigInt(LIMIT)) |> Seq.length
    printfn $"There are {result} values over {LIMIT:N0}"
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0