module Euler52

(*

*)

open System
open Strings

let sixFoldPermutation n =
    let str = n.ToString()
    isPermutation str (string (n*2)) &&
    isPermutation str (string (n*3)) &&
    isPermutation str (string (n*4)) &&
    isPermutation str (string (n*5)) &&
    isPermutation str (string (n*6)) 

        

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    //let N = 125874
    //printfn $"{N} and 2x{N} are permutations of each other: {isPermutation (string N) (string (N*2))}"

    let integer = Seq.initInfinite (fun n -> n+1) |> Seq.find sixFoldPermutation
    printfn $"The integer is {integer}"
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0