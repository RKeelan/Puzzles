module Util

open System
open LanguagePrimitives

// Unchecked Operators ----------------------------------------------------------------------------

let inline ( +! ) x y = Microsoft.FSharp.Core.Operators.(+) x y
let inline ( *! ) x y = Microsoft.FSharp.Core.Operators.(*) x y

// Modular Arithmetic -----------------------------------------------------------------------------

let inline modAdd x y modulus = (x + y) % modulus
let inline modMul x y modulus = (x * y) % modulus

// Tuple Helpers ----------------------------------------------------------------------------------

let firstOf3 (a, _, _) = a
let secondOf3 (_, b, _) = b
let thirdOf3 (_, _, c) = c

// Miscellaenous ----------------------------------------------------------------------------------

let inline product(s : seq<'a> when (^a) : (static member (*) : ^a * ^a -> ^a)) =
    match s with
    | sequence when Seq.isEmpty sequence -> GenericZero
    | _ -> s |> Seq.reduce (fun acc n -> acc*n)

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