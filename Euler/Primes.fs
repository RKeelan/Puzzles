module Primes

open System
open System.Linq
open Numbers

let isPrime p =
    match p with
    // RK 21-Dec-2021: Based on the answer to Euler #27, Project Euler doesn't consider negative
    // numbers to be prime
    | m when m < 0 -> false
    | 0 | 1 | 4 | 6 | 8 | 10 -> false
    | 2 | 3 | 5 | 7 | 11 -> true
    | _ ->
        let divisors = [2 .. (ceilSqrt p)]
        //printfn "%A" divisors
        not (isDivisibleByAny p divisors)

let isPrime64 p =
    match p with
    // RK 21-Dec-2021: Based on the answer to Euler #27, Project Euler doesn't consider negative
    // numbers to be prime
    | m when m < 0L -> false
    | 0L | 1L | 4L | 6L | 8L | 10L -> false
    | 2L | 3L | 5L | 7L | 11L -> true
    | _ ->
        let divisors = [2L .. (ceilSqrt p)]
        //printfn "%A" divisors
        not (isDivisibleByAny p divisors)

let nextPrime start =
    // RK 14-Dec-2021: Use the match to handle various corner cases
    match start with
    | i when i < 2 -> 2
    | 2 -> 3
    | _ ->
        // RK 14-Dec-2021: This is a bit controverial, but I feel like calling "nextPrime" on a
        // prime number should return the next prime after that number, not that number
        let firstNonPrime = if isPrime start then (start + 1) else start
        let sequenceStart = if isEven firstNonPrime then (firstNonPrime + 1) else firstNonPrime
        let candidates = Seq.initInfinite (fun n -> n*2 + sequenceStart)
        Seq.head (candidates |> Seq.filter (fun n -> isPrime n))

let primes =
    2 // First prime
    |> Seq.unfold (fun state -> Some(state, nextPrime state))
    

let rec nthPrime n = Seq.head(primes.Skip(n - 1))

// Return all primes up to n
let naiveSieve n =
    // From https://stackoverflow.com/questions/4629734/the-sieve-of-eratosthenes-in-f
    let rec sieve list =
        match list with
        | head::tail -> head :: (sieve (List.filter (fun x -> x % head <> 0) tail))
        | [] -> []
    sieve (2::[3 .. 2 .. n])

// Return all primes between n and m inclusive
let primesBetween n m = naiveSieve m |> List.filter (fun p -> p >= n)

let factorize number =
    match number with
    | z when z < 0 -> raise(ArgumentException("I'm not sure if and how to handle negative numbers"))
    | 0 -> raise(ArgumentException("I'm not sure how to factorize 0"))
    | 1 -> []
    | _ ->
        //wheel = [|2;3;5|]
        let inc = [|4;2;4;2;4;6;2;6|]
    
        //let rec getFactors n f =
        //    if isDivisible n f
        //    then f :: getFactors (n/f) f
        //    else []

        let mutable factors = []
        let mutable n = number
    
        while isDivisible n 2 do
            factors <- 2 :: factors
            n <- n/2
        
        while isDivisible n 3 do
            factors <- 3 :: factors
            n <- n/3
        
        while isDivisible n 5 do
            factors <- 5 :: factors
            n <- n/5

        let mutable k = 7
        let mutable i = 0
        while (k*k) <= n do
            if isDivisible n k then
                factors <- k :: factors
                n <- n / k
            else
                k <- k + inc.[i]
                i <- (i + 1) % inc.Length
        if n > 1 then do factors <- n :: factors
        factors