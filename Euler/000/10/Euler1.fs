module Euler1

(*
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
https://projecteuler.net/problem=1
*)

open System

let isMultipleOfThreeOrFive n = 
    if ((n % 3) = 0) || ((n% 5) = 0)
    then n
    else 0

let sumOfMultiplesBelow n =
    [1..n-1] |> List.map isMultipleOfThreeOrFive |> List.sum

//[<EntryPoint>]
let main argv =
    let number = sumOfMultiplesBelow 1000
    printf "%d" number
    0