﻿module Util

open System
open System.Linq
open LanguagePrimitives

// Tuple Helpers ----------------------------------------------------------------------------------

let firstOf3 (a, _, _) = a
let secondOf3 (_, b, _) = b
let thirdOf3 (_, _, c) = c

// Math Helpers -----------------------------------------------------------------------------------

// Return the lowest multiple of x that's less than or equal to y, and the factor that produced it
let inline lowestMultiple x y =
    let mutable result = GenericZero
    while result <= y do
        result <- result + x

    // When we break out of the while loop result is the first multiple of x greater than y, so
    // subtract one x
    let multiple = result - x
    let factor = multiple / x
    (multiple, factor) 

// RK 29-Dec-2021: This means reduce the sequence using reduce and the "*" operator
let inline factorial n = 
    match n with
    | m when m = GenericZero -> GenericZero
    | _ -> seq {GenericOne .. n} |> Seq.reduce (*)

let inline summands n = seq { for i in GenericZero .. (n/2) ->  (i,n-i) }

let inline ceilSqrt n = int (ceil (sqrt (float n)))
let inline floorSqrt n = int (floor (sqrt (float n)))

/// Rerturns true is n is divisible by d
let inline isDivisible n d = ((n % d) = GenericZero)

let inline divisorsNaive n =
    seq { for i in GenericOne .. n -> i }
    |> Seq.filter (fun i -> isDivisible n i)
    
// TODO Make this generic.
let inline divisors n =
    match n with
    | 1 -> seq { 1 }
    | 2 -> seq { 1; 2 }
    | _ -> seq { for i in GenericOne .. floorSqrt(n) do
                 if isDivisible n i then
                    let divisor = i
                    let quotient = n/i
                    yield i
                    if divisor <> quotient then yield quotient }

let inline divisorsExSelf n = divisors n |> Seq.filter (fun m -> m <> n)

let inline factors n =
    match n with
    | 1 -> seq { (1,1) }
    | 2 -> seq { (1,2) }
    | _ -> seq { for i in GenericOne .. floorSqrt(n) do
                 if isDivisible n i then yield (i, n/i)}

// TODO Make this generic.
let inline isEven n = isDivisible n 2
let inline isEven64 n = isDivisible n 2L

let inline isDivisibleByAll n list =
    let rec isDivisibleByAll n list =
        match list with
        | [] -> true
        | _ -> if (isDivisible n list.Head) then isDivisibleByAll n list.Tail else false
    isDivisibleByAll n list
    
let inline isDivisibleByAny n list =
    let rec isDivisibleByAny n list =
        match list with
        | [] -> false
        | _ -> if (isDivisible n list.Head) then true else isDivisibleByAny n list.Tail
    isDivisibleByAny n list

let inline product(s : seq<'a> when (^a) : (static member (*) : ^a * ^a -> ^a)) =
    match s with
    | sequence when Seq.isEmpty sequence -> GenericZero
    | _ -> s |> Seq.reduce (fun acc n -> acc*n)
    
let isInteger (d:double) = d%1.=0.

// String Helpers ---------------------------------------------------------------------------------

let hasDuplicates (s:string) : bool =
    let set = s.ToCharArray() |> Set.ofSeq
    set.Count < s.Length

let rotate (s:string) = s.Substring(1) + s.[0].ToString()

let rotations s = seq {
    yield s
    yield! rotate s |> Seq.unfold (fun str ->
        match str with
        | original when original = s -> None
        | _ -> Some((str, rotate str)))
    }

let isPalindrome (s:string) =
    let frontHalf = s.Substring(0, s.Length/2)
    let backHalfStart = if isEven s.Length then s.Length/2 else s.Length/2 + 1
    let backHalf = new string(s.Substring(backHalfStart) |> Seq.rev |> Seq.toArray)
    frontHalf = backHalf

let isPandigital (s:string) d = not(s.Contains('0')) && (s.Length = d) && (not (hasDuplicates s))

// Permutations -----------------------------------------------------------------------------------

let lexicographicPermutations (s:string) : list<string> =
    let chars = s.ToCharArray()
    Array.Sort chars

    let mutable result = [new string(chars)]
    let mutable permuting = true
    while permuting do
        let mutable k = -1
        for i in 0..chars.Length-2 do
            if chars.[i] < chars.[i+1] then do k <- i
            
        if k = -1 then do permuting <- false
        else
            let mutable l = -1
            for i in (k+1)..(chars.Length-1) do
                if chars.[k] < chars.[i] then do l <- i
            let c = chars.[k]
            chars.[k] <- chars.[l]
            chars.[l] <- c
            Array.Reverse(chars, (k+1), (chars.Length - 1 - k))
            result <- (new string(chars)) :: result
    result |> List.rev 

// Miscellaenous ----------------------------------------------------------------------------------

let printArray (row : 'a[]) = printfn "%s" (String.Join(" ", row))

let print2DArray (array : 'a[][]) =
    for row in array do
        printfn "%s" (String.Join(" ", row))

// From https://stackoverflow.com/a/1506343
// TODO Make this generic. See here for a lead on how to to do this:
// https://stackoverflow.com/a/15008816
let rec exceptSorted a b =
    let rec loop acc a b =
        match (a, b) with
        | [], x | x, [] -> (List.rev acc) @ x
        | x::xs, y::ys ->
            if x < y then loop (x::acc) xs (y::ys)
            elif x > y then loop (y::acc) (x::xs) ys
            else loop acc xs ys
    loop [] a b