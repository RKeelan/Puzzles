module Euler49

(*
The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual
in two ways:
    (i) each of the three terms are prime, and,
    (ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this
property, but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this sequence?
*)

(*
- One approach is, for each 4-digit prime, check if it's permutations are also prime
    - This sucks because there are a lot more than just four permutations
- Another approach is to enumerate all 4-digit primes, then try to find 4 among the set that are
    permutations of each another
- A bit late, I realize that "arithmetic sequence" isn't just a fancy way of saying "list"
    - It specifically means that the difference between terms is and must be constant
*)

open System
open Strings
open Primes

// Okay, well now I can find the one example...
let findArithmeticSequence n set =
    let sortedList = Set.toList set |> List.sort
    
    let mutable result = List.empty
    for m in sortedList do
        let diff = m - n
        let l = m + diff
        if List.contains l sortedList then result <- [n; m; l]

    if List.isEmpty result then None
    else Some(result)

let rec search primeSet : list<list<int>> =
    if Set.isEmpty primeSet then []
    else
        let p = Set.minElement primeSet
        let residual = Set.remove p primeSet
        let permutations = (residual
                         |> Set.filter (fun n -> isPermutation (n.ToString()) (p.ToString())))
                         //|> Set.add p

        let arithmeticSequence = findArithmeticSequence p permutations
        if arithmeticSequence.IsSome then
            arithmeticSequence.Value :: search residual
        else
            search residual

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let primutations = primesBetween 1000 9999
                    |> List.filter (fun n -> n.ToString().ToCharArray() |> Array.length = 4)
                    |> Set.ofList
                    |> search
    primutations |> List.iter (fun l -> printfn "%s: %s" (String.Join(", ", l)) (String.Join("", l)))

    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0