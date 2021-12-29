module Euler36

(*
The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

(Please note that the palindromic number, in either base, may not include leading zeros.)
*)

open System
open Strings

let isDoubleBasePalindrome (n:int) =
    let isBaseTenPalindrome = isPalindrome (n.ToString())
    let isBaseTwoPalinrome = isPalindrome (Convert.ToString(n,2))
    //printfn $"{n} in binary is {Convert.ToString(n,2)}"
    isBaseTenPalindrome && isBaseTwoPalinrome

let doubleBasePalindromes n =
    seq {1..n}
    |> Seq.filter (fun n -> not (n.ToString().EndsWith('0')))
    |> Seq.filter isDoubleBasePalindrome

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    //printfn $"585 is a double-base palindrome: {isDoubleBasePalindrome 585}"

    let N = 999999
    let palindromes = doubleBasePalindromes N |> Seq.toList
    let sum = palindromes |> List.sum
    printfn "%s" (String.Join('\n', palindromes))
    printfn $"The sum of the {palindromes.Length} double-base palindromes under {(N+1):N0} is {sum}"
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0