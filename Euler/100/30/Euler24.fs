module Euler24

(*
A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation
of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically,
we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:



What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
*)

open System
open Util

let permutations10 : seq<list<int>> = seq {
    let n = 9
    let choose = [0 .. n]
    for a in 0 .. n do
        let bChoice = List.except [a] choose
        for b in bChoice do
            let cChoice = List.except [b] bChoice
            for c in cChoice do
                let dChoice = List.except [c] cChoice
                for d in dChoice do
                    let eChoice = List.except [d] dChoice
                    for e in eChoice do
                        let fChoice = List.except [e] eChoice
                        for f in fChoice do
                            let gChoice = List.except [f] fChoice
                            for g in gChoice do
                                let gChoice = List.except [g] gChoice
                                for h in gChoice do
                                    let iChoice = List.except [h] gChoice
                                    for i in iChoice do
                                        let jChoice = List.except [i] iChoice
                                        for j in jChoice ->
                                            [a; b; c; d; e; f; g; h; i; j]
    }

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let p10 = permutations10 |> Seq.filter (fun s -> not (s.IsEmpty)) |> Seq.take 1000000
    let stringP10 = p10 |> Seq.map (fun s -> String.Join("", s))
    let millionthPermutation = stringP10 |> Seq.skip 999999 |> Seq.take 1 |> Seq.head
    printfn $"The millionth permutation is {millionthPermutation}" // 2783915460
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0