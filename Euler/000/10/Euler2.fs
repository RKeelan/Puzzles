module Euler2

open System

[<Literal>]
let limit = 4000000

let rec fibonacciEvenSum a b sum =
    printfn $"a = {a}, b = {b}, sum = {sum}"

    let newSum = 
        if((b % 2) = 0) then sum + b
        else sum

    if((a + b) > limit) then newSum
    else
        fibonacciEvenSum b (a+b) newSum

[<EntryPoint>]
let main argv =
    let number = fibonacciEvenSum 1 2 0
    printfn $"The result is {number}"
    0