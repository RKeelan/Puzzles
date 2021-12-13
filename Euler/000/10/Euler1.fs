module Euler1

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