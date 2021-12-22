module Euler17

(*
If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are
3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many
letters would be used?


NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23
letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out
numbers is in compliance with British usage.
https://projecteuler.net/problem=17
*)

(*
1. Enumerate the relevant numbers-as-strings
2. Combine them into one large string, then convert that to a char array
3. Filter spaces and hypens
4. Print the length of the result

But how to we convert from numbers to words?

* For numbers below 20, I think it's probably good to just hard-code the conversion.
* For [20 - 99], you want to get a word for the "tens" digit (i.e., twenty, thirty, etc) and
combine it with the digit that follows:
    * twenty-one
    * thirty-three
* For numbers where all but one digit are 0 (e.g., 100, 200, 1000), you want to append the word for
the most significant digit to the word for the remainder
    * This is already shaping up to be a lot of typing, so rather than trying to generalize past
1000, I'll just handle it as a special case
    *
*)

open System

let rec toWords n = 
    match n with
    | 0 -> ""
    | 1 -> "one"
    | 2 -> "two"
    | 3 -> "three"
    | 4 -> "four"
    | 5 -> "five"
    | 6 -> "six"
    | 7 -> "seven"
    | 8 -> "eight"
    | 9 -> "nine"
    | 10 -> "ten"
    | 11 -> "eleven"
    | 12 -> "twelve"
    | 13 -> "thirteen"
    | 14 -> "fourteen"
    | 15 -> "fifteen"
    | 16 -> "sixteen"
    | 17 -> "seventeen"
    | 18 -> "eighteen"
    | 19 -> "nineteen"
    | 20 -> "twenty"
    | 30 -> "thirty"
    | 40 -> "forty"
    | 50 -> "fifty"
    | 60 -> "sixty"
    | 70 -> "seventy"
    | 80 -> "eighty"
    | 90 -> "ninety"
    | m when n >= 20 && n <= 99 ->
        let remainder = m%10
        // It's not necessary to check if remainder is 0, because the cases above will take
        // precedence, but this shows the intent, and avoids relying on implementation details
        if remainder = 0 then $"{toWords (m-remainder)}"
        else $"{toWords (m-remainder)}-{toWords remainder}"

    // RK 17-Dec-2021: I'm relying on the fact that the patterns above will take precedence over
    // the one below
    | m when n >= 100 && n <= 999 -> 
        let hundredsDigit = m/100
        let remainder = m%100
        if remainder = 0 then $"{toWords hundredsDigit}-hundred"
        else $"{toWords hundredsDigit}-hundred and {toWords remainder}"

    | 1000 -> "one thousand"
    | _ -> raise (ArgumentException($"{n} not supported."))

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    let chars342 = ((toWords 342).ToCharArray() |> Array.filter (fun c -> Char.IsLetter(c))).Length
    printfn $"{chars342} letters in 342."
    
    let chars115 = ((toWords 115).ToCharArray() |> Array.filter (fun c -> Char.IsLetter(c))).Length
    printfn $"{chars115} letters in 115."
    
    let firstNumber = 1
    let lastNumber = 1000
    let numbersAsWords = seq { for n in firstNumber .. lastNumber -> toWords n }
    //Seq.iter (fun s -> printfn "%s" s) numbersAsWords
    let allNumbers = String.Join(",", numbersAsWords)
    let letters = allNumbers.ToCharArray() |> Array.filter (fun c -> Char.IsLetter(c))
    //printfn "%s" (String.Join("", letters))
    printfn $"{letters.Length} letters in the numbers from 1 to 1000."
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0