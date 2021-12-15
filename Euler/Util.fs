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