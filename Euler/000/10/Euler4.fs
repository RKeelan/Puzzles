module Euler4

(*
A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
https://projecteuler.net/problem=4
*)

open System
open System.Linq

[<Literal>]
let upperLimit = 999
let lowerLimit = 100


let isPalindrome (x, y, p) =
    let s = p.ToString()
    let s_reverse = new string(s.Reverse().ToArray())
    s.Equals(s_reverse)

// [<EntryPoint>]
let main argv =
    let products = seq {
        for i = upperLimit downto lowerLimit do
        for j = i downto lowerLimit do
            yield (i,j,i*j)
    }
    
    //let printTuple (x, y, p) = printfn $"({x}, {y}, {p})"
    //printfn $"{90109} is a palindrome: {isPalindrome 90109}"
    //printfn $"{1991} is a palindrome: {isPalindrome 1991}"

    //let _ = products.Take 1000 |> Seq.toList |> List.map printTuple
    
    //let largestPalindrome = Seq.head palindromeProducts
    //let palindromeProducts = products |> Seq.filter (fun (x, y, p) -> isPalindrome (x, y, p))
    //let largestPalindrome = Seq.head palindromeProducts
    //printfn $"The largest palindrome is {largestPalindrome}"

    let palindromeProducts = products |> Seq.filter (fun (x, y, p) -> isPalindrome (x, y, p)) |> Seq.toList

    let result = palindromeProducts |> List.sortBy (fun (x, y, p) -> -p) |> List.head
    printfn $"The largest palindrome is {result}"

    0
