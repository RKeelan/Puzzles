module Util

open System
open System.Linq

// Math Helpers -----------------------------------------------------------------------------------

let ceilSqrt n = int (ceil (sqrt (float n)))
let floorSqrt n = int (floor (sqrt (float n)))

let isDivisible n d = ((n % d) = 0)

let isEven n = isDivisible n 2

let rec isDivisibleByAll n list =
    match list with
    | [] -> true
    | _ -> if (isDivisible n list.Head) then isDivisibleByAll n list.Tail else false
    
let rec isDivisibleByAny n list =
    match list with
    | [] -> false
    | _ -> if (isDivisible n list.Head) then true else  isDivisibleByAny n list.Tail

// Primes -----------------------------------------------------------------------------------------

let isPrime p =
    match p with
    | 1 | 4 | 6 | 8 | 10 -> false
    | 2 | 3 | 5 | 7 | 11 -> true
    | _ ->
        let divisors = [2 .. (ceilSqrt p)]
        //printfn "%A" divisors
        not (isDivisibleByAny p divisors)

let nextPrime start =
    // RK 14-Dec-2021: Use the match to handle various corner cases
    match start with
    | i when i < 2 -> 2
    | 2 -> 3
    | _ ->
        // RK 14-Dec-2021: This is a bit controverial, but I feel like calling "nextPrime" on a prime
        // number should return the next prime after that number, not that number
        let firstNonPrime = if isPrime start then (start + 1) else start
        let sequenceStart = if isEven firstNonPrime then (firstNonPrime + 1) else firstNonPrime
        let candidates = Seq.initInfinite (fun n -> n*2 + sequenceStart)
        Seq.head (candidates |> Seq.filter (fun n -> isPrime n))

let primes =
    2 // First prime
    |> Seq.unfold (fun state -> Some(state, nextPrime state))
    

let rec nthPrime n = Seq.head(primes.Skip(n - 1))

// Miscellaenous ----------------------------------------------------------------------------------

// From https://stackoverflow.com/a/1506343
let rec exceptSorted a b =
    let rec loop acc a b =
        match (a, b) with
        | [], x | x, [] -> (List.rev acc) @ x
        | x::xs, y::ys ->
            if x < y then loop (x::acc) xs (y::ys)
            elif x > y then loop (y::acc) (x::xs) ys
            else loop acc xs ys
    loop [] a b