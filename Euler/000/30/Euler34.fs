module Euler34

(*
145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of their digits.

Note: As 1! = 1 and 2! = 2 are not sums they are not included.

https://projecteuler.net/problem=34
*)

(*
- What is the largest number where sum of digit factorials is plausible?
    - 9! = 362,880
    - So if you had, say, a six digit number, and you added another digit, it'd add 1 million to
    the number value, but only 362,880 to the sum of digits value
    - But the six digits you started with might already equal more than 1 million, so it's probably
    best to consider up to 7-digit numbers
- I don't think there's an easy lower bound, because 1!, 2!, and 3! are so small
*)

(*
- I was able to check all seven digit numbers in about 6 seconds, but I only found 145 (the example
number)
- Just in case, I checked, and "145" is not the answer
- Checking up to all 8 digit numbers took 65s, but I still didn't found any new numbers
- I'm really not sure what to make of this
    - Normally I'd suppose that my algorithm is wrong, but it correctly finds 145
    - I could be wrong about thinking that I can put an upper bound on the possible numbers, but
        - I'm pretty sure I'm not
        - Brute-forcing really large numbers is really contrary to the spirit of ProjectEuler.net
    - All of my code is using int64, so I don't see how this could be issue with overflow
    - I've double- and triple-checked my factorial lookup table
    - I spot checked a few number/digit factorial pairs and didn't find any issues
    - I guess my next step is to make an excel spreadsheet and see if it can kind any such numbers,
    then run that through my code to see what's breaking down
- RK 27-Dec-201: The problem was that 0! is 1, not 0
*)

open System

let digitFactorial c =
    match c with
    | '0' -> 1L
    | '1' -> 1L
    | '2' -> 2L
    | '3' -> 6L
    | '4' -> 24L
    | '5' -> 120L
    | '6' -> 720L
    | '7' -> 5040L
    | '8' -> 40320L
    | '9' -> 362880L
    | _ -> raise(ArgumentException($"{c} is not a number"))

let sumOfDigitFactorials (n:int64) = n.ToString().ToCharArray() |> Seq.map digitFactorial |> Seq.sum

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    let N = 9999999L
    let curiousNumbers = seq {10L..N} |> Seq.filter (fun n -> n = sumOfDigitFactorials n)
    printfn "%s" (String.Join('\n', curiousNumbers))
    let sum = curiousNumbers |> Seq.sum
    printfn $"The sum of digit factorials of 9999999 is {sum}"
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0