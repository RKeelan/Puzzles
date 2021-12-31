module Euler48

(*
The series, 11 + 22 + 33 + ... + 1010 = 10405071317.

Find the last ten digits of the series, 11 + 22 + 33 + ... + 10001000.
https://projecteuler.net/problem=48
*)

(*
- Sadly, uncheck multiplication doesn't seem to roll over properly, and calculating large powers
purely using addition is too slow (not that I'm even sure unchecked addition will work)
- Having thought some more about, I think I understand why uncheked multiplication didn't work
    - Unchecked multiplication means the values wrap-around after getting larger than the container
    (64 bits, in my case)
    - This is the same as doing modular arithmetic with mod 2^64
    - But If I want to preserve the last ten digits, I need to base-ten module arithmetic
- But what modulus do I need to preserve ten digits? In my uni tests for modMul,
    - 10 preserves the last digit
    - 10,000,000 preserves a 7 digit number
    - So I probably need an 11 digit numbet
*)

open System
open LanguagePrimitives
open Util

let selfPower n modulus =
    let rec pow acc x = 
        match x with
        | y when y = GenericZero -> acc
        | _ -> pow (modMul n acc modulus) (x-GenericOne)
    pow GenericOne n
    
let modulus = 10000000000UL

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    let integer = seq{1UL..1000UL}
                |> Seq.map (fun n -> selfPower n modulus)
                |> Seq.reduce (fun x y -> modAdd x y modulus)
    let number = integer.ToString()
    let digits = number.Substring(number.Length-10)
    printfn $"The last ten digits are {digits}"
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0