module Euler5

(*
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
https://projecteuler.net/problem=5
*)

open System
open System.Linq
open Util

//[<EntryPoint>]
let main argv =
    let divisors = [2 .. 20]

    // Only need to try every 10th number, because we know the numbers between won't be evenly
    // divisible by 10
    let numbers = Seq.initInfinite (fun n -> n*10 + 10)
    let _ = numbers.Take 10 |> Seq.toList |> printfn "%A" 
    let smallestMulitple = numbers |> Seq.filter (fun n -> Util.isDivisibleByAll n divisors) |> Seq.head

    printfn $"{smallestMulitple}"
    0