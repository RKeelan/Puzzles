module Euler38

(*
Take the number 192 and multiply it by each of 1, 2, and 3:

192 × 1 = 192
192 × 2 = 384
192 × 3 = 576
By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the
concatenated product of 192 and (1,2,3)

The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the
pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).

What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product
of an integer with (1,2, ... , n) where n > 1?
https://projecteuler.net/problem=38
*)

(*
- We definitely don't need to consider any number larger than 9 digits
- In fact, we probably don't need to consider a number larger than 4 digits, because a 5 digit
number would give 10 digits when the 1- and 2-products were concatenated
- For the concatenated product, we look at each successive n until we find a duplicate digit, or
we get a 1-9 pandigital number
    - Sadly, this means I probably won't need that pandigital function I added to Util. Oh well
*)

open System

let rec pandigitalMultipleRec number n (acc:string) : option<string> = 
    if acc.Contains('0') then None
    elif Util.hasDuplicates acc then None
    elif acc.Length = 9 then Some(acc)
    else pandigitalMultipleRec number (n+1) (acc+((number*n).ToString()))
    

let pandigitalMultiple number =
    let str = pandigitalMultipleRec number 1 ""
    if str.IsNone then None
    else Some(Int64.Parse(str.Value))

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let p = pandigitalMultiple 192
    printfn $"The pandigital multiple of 192 is {p}"

    let N = 9999
    let all = seq {1..N} |> Seq.map pandigitalMultiple |> Seq.choose id |> Seq.toList
    printfn $"The pandigital multiples are:"
    printfn "%s" (String.Join('\n', all))
    let largest = all |> Seq.max
    printfn $"The largest is {largest}"
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0