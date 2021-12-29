module Euler42

(*
The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so the first ten
triangle numbers are:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

By converting each letter in a word to a number corresponding to its alphabetical position and
adding these values we form a word value. For example, the word value for SKY is
    19 + 11 + 25 = 55 = t10.

If the word value is a triangle number then we shall call the word a triangle word.

Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly
two-thousand common English words, how many are triangle words?
*)

(*
- Copy letter score and word score from Euler22 to Util
- Two approaches:
    1. For every wordscore, check if it's a triangle number
    2.  (a) Find the highest word score
        (b) Compute all triangle numbers less than or equal to that score and put them in a Set
        (c) For every word score, check if the score is in the set of triangle numbers
- I was going to do 2 (and even wrote some code for it), but I realized that I probably wouldn't
save any time that way. Approach 1 only took 139 ms
*)

open System
open System.IO
open Numbers
open Strings

///[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    // File is a single line, with words enclosed in quotation marks and separated by commas
    let line = Seq.head (File.ReadLines @"..\..\..\000\40\Euler42.txt")
    let words = line.Split([|',';'"'|], StringSplitOptions.RemoveEmptyEntries)
    let wordScores = words |> Seq.map wordScore
    let triangleWords = wordScores |> Seq.filter isTriangleNumber |> Seq.toList
    printfn $"There are {triangleWords.Length} triangle words"
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0