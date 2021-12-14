module Euler7

(*
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?
https://projecteuler.net/problem=7
*)


open Util

//[<EntryPoint>]
let main argv =
    let n = 10001
    printfn $"Prime #{n} = { Util.nthPrime n}"
    0