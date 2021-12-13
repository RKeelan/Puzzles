module Euler3

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