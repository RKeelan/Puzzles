module Util

open System
open System.Linq
open LanguagePrimitives

// Math Helpers -----------------------------------------------------------------------------------

let inline ceilSqrt n = int (ceil (sqrt (float n)))
let inline floorSqrt n = int (floor (sqrt (float n)))

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

// TODO Make this generic.
let inline isEven n = isDivisible n 2


// TODO Make this generic. See here for a lead on how to to do this:
// https://stackoverflow.com/a/15008816
let rec isDivisibleByAll n list =
    match list with
    | [] -> true
    | _ -> if (isDivisible n list.Head) then isDivisibleByAll n list.Tail else false
    
let rec isDivisibleByAny n list =
    match list with
    | [] -> false
    | _ -> if (isDivisible n list.Head) then true else  isDivisibleByAny n list.Tail

let inline product(s : seq<'a> when (^a) : (static member (*) : ^a * ^a -> ^a)) =
    match s with
    | sequence when Seq.isEmpty sequence -> GenericZero
    | _ -> s |> Seq.reduce (fun acc n -> acc*n)

// Miscellaenous ----------------------------------------------------------------------------------

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