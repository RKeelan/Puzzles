module Euler16

(*
2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?
https://projecteuler.net/problem=16
*)

(*
I think my approach will be to take 1 and double it 1k times using my own function that handles the
overflow

*)

open System

// The largest number a single element in the list can represent
[<Literal>]
let COMPONENT_MAX = 999999999L
let FORMAT_STRING = COMPONENT_MAX.ToString().Replace('9', '0')

let rec multiply (multiplicand : list<int64>) (multiplier : int64) (carryOver : int64) =
    match multiplicand with
    // The least-significant element is the head of the list, so if we get to the end, return
    // either nothing or the carry-over
    | [] ->
        //printfn $"\t\tFinished doubling. Final carry-over is {carryOver:N0}"
        if carryOver = 0L then [] else [carryOver]
    | head::tail ->
        let newHead =
            if (head * 2L) > COMPONENT_MAX then
                (head * 2L + carryOver) % (COMPONENT_MAX + 1L)
            else (head * 2L + carryOver)
        let newCarryOver = (head * 2L) / (COMPONENT_MAX + 1L)
        //printfn $"\tDoubled {head:N0} to {newHead:N0} with {newCarryOver:N0} carry-over"
        newHead :: (multiply tail multiplier newCarryOver)

// Double "number" n times
let rec double (number : list<int64>) (n : int64) =
    let number2 = multiply number 2L 0L
    match n with
    | 1L -> number2
    | _ -> double number2 (n-1L)

let sumOfDigits (digits : string) : int64 =
    Array.fold (fun (acc:int64) (c:char) -> acc + Int64.Parse(c.ToString())) 0L (digits.ToCharArray())

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let numberOfDoublings = 1000L
    let n = 1L
    let number = [n]
    printfn $"Doubling {n} {numberOfDoublings} times"
    let resultList = double [1L] numberOfDoublings
    let resultString = String.Join("", List.rev(resultList) |> List.map (fun n -> n.ToString("000")))
    let sumOfDigits = sumOfDigits resultString
    stopWatch.Stop()
    printfn "The result as a list is %A" resultList
    printfn $"The result as a number is {resultString}" 
    printfn $"The sum of the digits is {sumOfDigits}"
    
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0