module Combinatronics

open System
open Numbers
open BigInt

let inline nCr n r = factorial(n)/(factorial(r)*factorial(n-r))

let inline NCR n r =
    let (q, r) = BigInt.bigFactorial(n) / ((BigInt.bigFactorial(r))*BigInt.bigFactorial(n-r))
    if r <> (BigInt.zero r.Radix) then raise <| ArgumentException("Remaineder was not zero")
    q
    
let rec words alphabet length =
    seq {
        match length with
        | 0 -> yield []
        | n ->
            for letter in alphabet do
                for word in (words alphabet (length-1)) do
                    yield letter::word
    }
