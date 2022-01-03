module Euler33

(*
The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to
simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling
the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction, less than one in value, and
containing two digits in the numerator and denominator.

If the product of these four fractions is given in its lowest common terms, find the value of the
denominator.

https://projecteuler.net/problem=33
*)

(*
- I'm not even sure that this is asking.
- If 49/98 = 4/8 and 30/50 are trivial examples, what are the non-trivial ones?
- Is 49/98 a non-trivial example?
- I'm going to proceed on the assumption that 30/50 is trivial because the digits being cancelled
are zeros, and that's actually kind of true
    - The other possibility I see is that an instance is only non-trivial if the digits beings
    cancelled are in different positions
*)

(*
1. I want every two-digit number pair that shares at least one digit (excluding 0s)
2. Then I...
    a) Divide the number pair, plus the pair with the duplicate digit removed?
        - Would I have to account for rounding errors?
        - I feel like actually I wouldn't
    b) 
*)

open System
open Util
open Numbers

let fractions =
    seq {
    for a in 12 .. 97 do
        for b in a+1 .. 98 do
            let aChars = a.ToString().ToCharArray()
            let bChars = b.ToString().ToCharArray()
            if (aChars.[0] = bChars.[0]) || (aChars.[0] = bChars.[1]) ||
               (aChars.[1] = bChars.[0]) || (aChars.[1] = bChars.[1]) then
                yield (a,b)
                
    }
    |> Seq.filter (fun (a,b) -> not (a.ToString().Contains('0') || b.ToString().Contains('0')))
    |> Seq.filter (fun (a,b) -> not ((isDivisible a 11) || (isDivisible b 11)))

// RK 26-Dec-2021: I don't need to worry about the case where both digits matcn (e.g., 19/91),
// because the value of the fraction after digit cancellation will be 1, which won't be true of the
// original pair, because I filter those numbers out beforehand
let digitsCancel (a,b) =
    let aChars = a.ToString().ToCharArray()
    let bChars = b.ToString().ToCharArray()
    let duplicateDigit =  if (aChars.[0] = bChars.[0]) || (aChars.[0] = bChars.[1]) then aChars.[0]
                          else aChars.[1]
    let aPrime = Int32.Parse(a.ToString().Replace(duplicateDigit.ToString(),""))
    let bPrime = Int32.Parse(b.ToString().Replace(duplicateDigit.ToString(),""))
    //printfn $"Comparing {a}/{b} to {aPrime}/{bPrime}"
    (double a/ double b) = (double aPrime / double bPrime)

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let candidates = fractions
    //printfn "%s" (String.Join('\n', candidates))
    let examples = candidates |> Seq.filter (fun x -> digitsCancel x)
    //printfn "%s" (String.Join('\n', examples))
    let productNumerator =  examples |> Seq.map fst |> product
    let productDenominator = examples |> Seq.map snd |> product
    printfn $"The product of the fractions is {productNumerator}/{productDenominator}"
    let numeratorDivisors = divisors productNumerator |> Set.ofSeq
    let denominatorDivisors = divisors productDenominator |> Set.ofSeq
    let commonDivisors = Set.intersect numeratorDivisors denominatorDivisors
    printfn $"These are the common divisors"
    printfn "%s" (String.Join('\n', commonDivisors))
    // The answer is 100, which I obtained manually using the greatest common divisor
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0