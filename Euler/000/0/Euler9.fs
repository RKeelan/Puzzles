module Euler9

(*
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a2 + b2 = c2
For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
https://projecteuler.net/problem=9
*)

(*
From https://en.wikipedia.org/wiki/Pythagorean_triple#Generating_a_triple, for m > n > 0

a = m^2 - n^2
b = 2*m*n
c = m^2 + n^2

*)

open System.Linq
open Numbers

let ascendingTuples i = seq {for j in 1 .. i do yield (i, j)}

//[<EntryPoint>]
let main argv =
    let pythagoreanTriplets =
        seq {
        for m in 2 .. 1000 do 
            for n in 1 .. m ->
                let a = (m*m) - (n*n)
                let b = 2*m*n
                let c = ceilSqrt((a*a) + (b*b))
                let s = a + b + c
                (a, b, c, s)
        }
    
    let _ = pythagoreanTriplets.Take(10) |> Seq.iter (fun (a, b, c, sum) ->
        printfn "%d + %d + %d = %d" a b c sum
    )
    
    // Find the tuple whose sum is 1000
    let result = pythagoreanTriplets.First(fun (a, b, c, sum) -> sum = 1000)
    
    // Destructure the result to get the product
    let (a, b, c, _) = result
    let product = a*b*c
    printfn $"{product}"
    0
    