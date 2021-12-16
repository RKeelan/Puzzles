module Euler14

(*
The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although
it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
https://projecteuler.net/problem=14
*)

(*
Approach:
1. Write a function that produces a Collatz Sequence. I think I can go this as a recursive function
2. For every Collatz sequence starting from 1 .. 999,999,999, produce a tuple (startNumber, length)
3. Find the max by length
*)

open System
open Util

type colltazLength = {
    start : int64
    length : int64
}

let printColltazSequence (s : seq<'a>) = printfn "%s" (String.Join(" -> ", s))

/// Collatz sequence starting from n
let collatzSequence n =
    n
    |> Seq.unfold(fun state -> match state with
                                | 0L -> None
                                | 1L -> Some(state, 0L)
                                | n when isEven64 n -> Some(state, state/2L)
                                | _ -> Some(state, 3L*state + 1L))

// A sequence of all collatz sequences from 1 to n
let collatzSequences n = seq { 
        for i in 1L .. n -> collatzSequence i
    }

let collatzPairs n = seq {
        for i in 1L .. n -> (i, Seq.length (collatzSequence i))
    }
    
//[<EntryPoint>]
let main argv =
    let first10 = collatzSequences 10L
    first10 |> Seq.iter printColltazSequence

    let n = 999999L
    let (start, length) = collatzPairs n |> Seq.maxBy snd
    
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    printfn $"The longest chain under {n} starts at {start} and contains {length} terms."
    stopWatch.Stop()
    printfn $"Execution time {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0