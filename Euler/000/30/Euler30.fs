module Euler30

(*
Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:

1634 = 14 + 64 + 34 + 44
8208 = 84 + 24 + 04 + 84
9474 = 94 + 44 + 74 + 44
As 1 = 14 is not a sum it is not included.

The sum of these numbers is 1634 + 8208 + 9474 = 19316.

Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

https://projecteuler.net/problem=30
*)

(*
- Is there some bound on this problem?
    - How do I know some random 50-digit number can't be written as the sum of it's digits?
    - Fifth-powers range from
        - 1^5 =      1
        - 2^5 =     32
        - 3^5 =    243
        - 4^5 =  1,024
        - 5^5 =  3,125
        - 6^5 =  7,776
        - 7^5 = 16,807
        - 8^5 = 32,768
        - 9^5 = 59,049
- Is there a reason the fourth-power numbers are all four digits?
- I think it has something to do with the largest fourth-power of a single digit (9^4 = 6561) being
four digits.
- If you add a fifth digit to a four digit number, even if it's 1, it will increase the "sum" by
10k, and there's no digit whose value is big enough
- Similarly, with fifth powers, the largest number is 9^5 = 59,049
- Actually, maybe this is better: at what point does a number consisting of all 9s outgrow this
possibility?
    - It's a 6-digit number
- So I'll check all 5-digit numbers
- Addendum: I tried calculating the powers fo all 6 digits numbers and found one
    - But I didn't find any 7 digit numbers
*)

open System

let sumOfdigitPowers (n:int) (m:int) =
    n.ToString()
    |> Seq.chunkBySize 1
    |> Seq.map (fun s -> Int32.Parse(s.[0].ToString()))
    |> Seq.map (fun d -> int32 (pown d m))
    |> Seq.sum

let all5thPowers =
    seq{ 2 .. 9999999 }
    |> Seq.filter (fun n -> n = (sumOfdigitPowers n 5))

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    //let mutable testInput = 1634
    //let mutable testOuput = sumOfdigitPowers testInput 4
    //printfn $"The sum of the fourth powers of the digits of {testInput} is {testOuput}"
    
    //testInput <- 8208
    //testOuput <- sumOfdigitPowers testInput 4
    //printfn $"The sum of the fourth powers of the digits of {testInput} is {testOuput}"
    
    //testInput <- 9474
    //testOuput <- sumOfdigitPowers testInput 4
    //printfn $"The sum of the fourth powers of the digits of {testInput} is {testOuput}"

    let fifthPowers = all5thPowers
    printfn "%s" (String.Join('\n', fifthPowers))
    let sum = fifthPowers |> Seq.sum
    printfn $"The sum of fith powers is {sum}"
    (*
    4150
    4151
    54748
    92727
    93084
    194979
    The sum of fith powers is 443839
    Execution time: 3,396 ms.
    *)

    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0