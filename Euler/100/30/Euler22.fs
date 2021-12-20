module Euler22

(*
Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over
five-thousand first names, begin by sorting it into alphabetical order. Then working out the
alphabetical value for each name, multiply this value by its alphabetical position in the list to
obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 +
9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?
https://projecteuler.net/problem=22
*)

open System
open System.IO

let letterScore (c:char) = (int64(c) - int64('A') + 1L)
let nameScore (s:string) = s.ToCharArray() |> Array.map letterScore |> Array.sum
let alphabetizedScore (i:int64) (s:string) = i * nameScore(s)

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    let c = 'C'
    printfn $"Letter score of {c} is {letterScore c}"
    let name = "COLIN"
    printfn $"Name score of {name} is {nameScore name}"
    let position = 938L
    printfn $"The name score of {name} at position {position} is {alphabetizedScore position name}"
    
    let line = Seq.head (File.ReadLines @"..\..\..\100\30\Euler22.txt")
    let names = line.Split([|',';'"'|], StringSplitOptions.RemoveEmptyEntries)
    let sortedNames = names |> Seq.sort |> Seq.toArray
    //File.WriteAllLines(@"..\..\..\100\30\Euler22-ProcessedNames.txt", sortedNames)
    let totalNameScore = seq { for i in 0 .. (sortedNames.Length - 1) -> alphabetizedScore (int64 (i + 1)) sortedNames.[i] } |> Seq.sum
    printfn $"The total name score is {totalNameScore}"

    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0