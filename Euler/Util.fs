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


#nowarn "0064"
(*
E.g., Warning FS0064 This construct causes code to be less generic than indicated by the type
annotations. The type variable 'T has been constrained to be type 'int'.

As far as I can tell, I've explicitly constrained 'T to a type that supports "*", so there should
be no issue, but apparently not.

(But note that I worked on this before adding the special handing for empty sequence)
*)

let product(s : seq<'T> when (^T) : (static member (*) : ^T * ^T -> ^T)) =
    match s with
    | sequence when Seq.isEmpty sequence -> 0
    | _ -> s |> Seq.reduce (fun (acc:'T) (n:'T) -> acc*n)

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