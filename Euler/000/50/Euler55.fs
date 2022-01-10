module Euler55

(*
If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.

Not all numbers produce palindromes so quickly. For example,

349 + 943 = 1292,
1292 + 2921 = 4213
4213 + 3124 = 7337

That is, 349 took three iterations to arrive at a palindrome.

Although no one has proved it yet, it is thought that some numbers, like 196, never produce a
palindrome. A number that never forms a palindrome through the reverse and add process is called a
Lychrel number. Due to the theoretical nature of these numbers, and for the purpose of this problem,
we shall assume that a number is Lychrel until proven otherwise. In addition you are given that for
every number below ten-thousand, it will either (i) become a palindrome in less than fifty
iterations, or, (ii) no one, with all the computing power that exists, has managed so far to map
it to a palindrome. In fact, 10677 is the first number to be shown to require over fifty iterations
before producing a palindrome: 4668731596684224866951378664 (53 iterations, 28-digits).

Surprisingly, there are palindromic numbers that are themselves Lychrel numbers; the first example
is 4994.

How many Lychrel numbers are there below ten-thousand?

NOTE: Wording was modified slightly on 24 April 2007 to emphasise the theoretical nature of Lychrel
numbers.
https://projecteuler.net/problem=55
*)

open System
open Strings
open BigInt

let reverse (n:BigInt) = BigInt.parse (Strings.reverse (n.ToString())) n.Radix

let lychrelSeq n =
    let bigN = new BigInt(int64 n, INT_64_RADIX)
    bigN |> Seq.unfold (fun n -> Some(n, (n + (reverse n))))

let isLychrel n =
    lychrelSeq n
    |> Seq.skip 1 //  Don't count the number itself
    |> Seq.take 50
    |> Seq.tryFind (fun n -> isPalindrome (n.ToString()))
    |> Option.isNone

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    //let n = 47
    //let copula = if isLychrel n then "is" else "is not"
    //printfn $"{n} {copula} a Lychrel number"
    
    //let n = 349
    //let copula = if isLychrel n then "is" else "is not"
    //printfn $"{n} {copula} a Lychrel number"
    
    //let n = 4994
    //let copula = if isLychrel n then "is" else "is not"
    //printfn $"{n} {copula} a Lychrel number"

    let N = 10000
    let lychrelNumbers = seq {1..(N-1)} |> Seq.filter isLychrel |> Seq.toList
    printfn $"There are {lychrelNumbers.Length} Lychrel numbers below {N}"
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0