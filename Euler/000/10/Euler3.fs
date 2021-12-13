module Euler3

(*
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
https://projecteuler.net/problem=3
*)

open System

[<Literal>]
let prime = 600851475143L
//let prime = 13195L

let rec primeFactors (factor:int64) (n:int64) list =
    printfn $"factor = {factor}, n = {n}, list = {list}"
    if (n = 1L) then
        list
    elif ((n % factor) = 0L) then
        primeFactors factor (n / factor) (factor :: list)
    else
        primeFactors (factor + 1L) n list

//[<EntryPoint>]
let main argv =
    let factors = primeFactors 2L prime []
    printfn $"The result is %A{factors}"
    0