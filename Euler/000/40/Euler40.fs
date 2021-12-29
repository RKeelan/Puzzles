module Euler40

(*
An irrational decimal fraction is created by concatenating the positive integers:

0.123456789101112131415161718192021...

It can be seen that the 12th digit of the fractional part is 1.

If dn represents the nth digit of the fractional part, find the value of the following expression.

d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
https://projecteuler.net/problem=40
*)

(*
- I think I want a function that takes an ordered list of digit positions and returns a product of the digits at those positions

*)

open System
open System.Collections.Generic
open Numbers

let champernownesDigits = seq {for i in 1..Int32.MaxValue do yield! i.ToString().ToCharArray() }

let takeAfter n (enumerator:IEnumerator<char>) =
    for j in 1..n do
        enumerator.MoveNext() |> ignore
    Int32.Parse(enumerator.Current.ToString())

let rec enumerateDigits (digitEnumerator:IEnumerator<char>) (skips:list<int>) : list<int> =
    match skips with
    | [] -> []
    | head::tail -> (takeAfter head digitEnumerator) :: (enumerateDigits digitEnumerator tail)

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let digitEnumerator = champernownesDigits.GetEnumerator()
    let digits = enumerateDigits digitEnumerator [1;9;90;900;9000;90000;900000]
    printfn $"The digits are:"
    printfn "%s" (String.Join("\n", digits))
    let product = product digits
    printfn $"The product of the digits is {product}"

   
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0