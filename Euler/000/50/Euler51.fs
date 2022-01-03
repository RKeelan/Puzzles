module Euler51

(*
By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible
values: 13, 23, 43, 53, 73, and 83, are all prime.

By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first
example having seven primes among the ten generated numbers, yielding the family:

56003, 56113, 56333, 56443, 56663, 56773, and 56993

Consequently 56003, being the first member of this family, is the smallest prime with this property.

Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits)
with the same digit, is part of an eight prime value family.
*)

(*
- We need the smallest prime, so we want to start our search at the low end
- One problem is that we have two directions of search:
    - Increase the "base" number
    - Change the number of digits that get replaced
- One thing I note is that if the digit(s) is(are) the left-most,they can't be replaced with 0
    - That being said, for every base, you have either 9 or 10 "chances", and we need a base that
    hits 8 of them
    - 
- For every number, I guess we need to check every distribution of wildcards except the one in
which every digit is a wildcard--because in that case, at best the "1111" version will be prime,
but none of the others will be
- I'm pretty sure that attacking this search as iterate over numbers from N (=2? 100?) up
- Instead, I feel like it needs to be a tree search or something:
    - In the following lines "*" is a replaceable digit, "d" is a fixed digit
    - I don't need to consider any pattern which ends with a "*", because none of the even-numbered
    digits, not '5', will yield primes
    - *d
    - d*d, **d
    - *ddd, d*dd, dd*d, **dd, d**d, ***d
    - *dddd, d*ddd, dd*dd, ddd*d, **ddd, d**dd, dd**d, ***dd, d***d, ****d
- In the pattterns above, there are roughly 10^(number of ds) "number" for each [d\*]+ pattern,
and for each "number" we need to produce a list of 9 or 10 numbers, each of which needs to be
checked for primality
- On thing that I think I'd like to do is precomputer a Set of primes, so that I don't have to run
the primality algorithm on each number
    - But I probably want to segment the number space, because lookups will get slower the larger
    the prime table is
- I was thinking of making an implementation that turned strings with ds *s into numbers, but I
feel like all the strings and parsing might kill my performance
    - It's noteworthy that the d*d sequence can be constructed by starting with d0d then adding 10
    ten times
*)

open System
open Primes

let getBounds d = ((pown 10 (d-1)), ((pown 10 d)-1))

let getPatternString (d:int) (p:int) = p.ToString($"D{d}").Replace('1','*').Replace('0','d')

let getReplacementPatterns d =
    //printfn $"Replacement patterns for {d} digit numbers"
    let set = ['0';'1'] // The order here is important. See below
    let patterns = Combinatronics.words set (d-1)
                |> Seq.tail // The first element will be all '0's, which we want to discard
                |> Seq.map (fun w -> Int32.Parse(new string(List.toArray w))*10)

    //patterns |> Seq.iter (fun p -> printfn "  %s" (String.Join("", p)))
    patterns

let rec getGenerators digits pattern =
    let isOnesAndZeros n =
        let mutable n = n
        while n > 10 do
            let r = n % 10
            if r <> 1 && r <> 0 then do
                n <- r // This will break out of the loop and return false afterwards
            else
                n <- n/10
        let r = n % 10
        ((r = 1) || (r = 0))

    let maskMatch d pattern =
        let mask = pown 10 d
        let difference = pattern - mask
        if difference < 0 then false
        elif difference = 0 then true
        else isOnesAndZeros difference

    // This yields all the "length" digit numbers excluding multiples of 10, which is not what I
    // intended when I started out, but which does work for me
    let rec getBases d =
        seq {
            match d with
            | 0 -> yield 0
            | _ ->
                if maskMatch (digits-d) pattern then
                    for number in (getBases (d-1)) do
                        yield number*10
                else
                    for i in [1..9] do
                        for number in (getBases (d-1)) do
                            yield  number*10 + i
        }
    
    getBases digits |> Seq.map (fun d -> (pattern, d))
    
let numberFamily pattern number =
    [
        // If we're generating a pattern with leading *s (e.g., *d*d), we can't yield the base
        // number, because it won't be the right number of digits. But in this case, pattern will
        // be larger than number (e.g., 1010 > 909
        if number > pattern then yield number
        yield! Seq.init 9 (fun i -> number + pattern * (i+1))
            
    ]

// Find the largest prime family of d digits
let findPrimeFamilies d =
    if d = 1 then do raise(ArgumentException("The concept of \"Prime family\" is undefined for single-digit numbers"))
    printfn $"Finding the prime families among {d} digit numbers"

    let replacementPatterns = getReplacementPatterns d

    // This code allows printing the patterns and raw families
    //let families = seq { for pattern in replacementPatterns do yield! getGenerators d pattern }
    //families |> Seq.iter (fun (pattern, number) -> printfn "Pattern: %s, Base: %d" (getPatternString d pattern) number)
    //let families = families |> Seq.map (fun (pattern, number) -> numberFamily pattern number)
    //families

    let families = seq { for pattern in replacementPatterns do yield! getGenerators d pattern }
                    |> Seq.map (fun (pattern, number) -> numberFamily pattern number)
    let primeFamilies = families
                        |> Seq.map (fun family -> family |> List.filter isPrime)
                        |> Seq.filter (fun f -> not (Seq.isEmpty f))
    primeFamilies

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    for d in [6..6] do
        let largePrimeFamilies = findPrimeFamilies d |> Seq.filter (fun family -> family.Length > 7)
        largePrimeFamilies |> Seq.iter (fun family -> printfn "Number family: %s" (String.Join(" ", family)))

    (*
    Finding the largest prime family in 6 digit numbers
    Number family: 121313 222323 323333 424343 525353 626363 828383 929393
    Execution time: 64,064 ms.
    *)
    
    //printfn $"{2} digit numbers with pattern {getPatternString 2 10}:"
    //let numbers = getGenerators 2 10
    //numbers |> Seq.iter (fun number -> printfn "%s" (String.Join(" ", snd number)))
    
    //printfn $"{3} digit numbers with pattern {getPatternString 3 110}:"
    //let numbers = getGenerators 3 110
    //numbers |> Seq.iter (fun number -> printfn "%s" (String.Join(" ", snd number)))
    
    //printfn $"{3} digit numbers with pattern {getPatternString 3 10}:"
    //let numbers = getGenerators 3 10
    //numbers |> Seq.iter (fun number -> printfn "%s" (String.Join(" ", snd number)))
    
    //printfn $"{4} digit numbers with pattern {getPatternString 4 1110}:"
    //let numbers = getGenerators 4 1110
    //numbers |> Seq.iter (fun number -> printfn "%s" (String.Join(" ", snd number)))
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0

(* Appendix A: Pattern analysis
Mask	Pattern	Result	
1	10	9	
10	10	0	<-
			
Mask	Pattern	Result	
1	10	9	
10	10	0	<-
100	10	-90	
			
Mask	Pattern	Result	
1	100	99	
10	100	90	
100	100	0	<-
			
Mask	Pattern	Result	
1	110	109	
10	110	100	<-
100	110	10	<-
			
Mask	Pattern	Result	
1	1110	1109	
10	1110	1100	<-
100	1110	1010	<-
1000	1110	110	<-
10000	1110	-8890	
100000	1110	-98890	
1000000	1110	-998890	
			
Mask	Pattern	Result	
1	10	9	
10	10	0	<-
100	10	-90	
1000	10	-990	
10000	10	-9990	
100000	10	-99990	
1000000	10	-999990	
			
Mask	Pattern	Result	
1	100	99	
10	100	90	
100	100	0	<-
1000	100	-900	
10000	100	-9900	
100000	100	-99900	
1000000	100	-999900	
			
Mask	Pattern	Result	
1	110	109	
10	110	100	<-
100	110	10	<-
1000	110	-890	
10000	110	-9890	
100000	110	-99890	
1000000	110	-999890	
			
Mask	Pattern	Result	
1	1000	999	
10	1000	990	
100	1000	900	
1000	1000	0	<-
10000	1000	-9000	
100000	1000	-99000	
1000000	1000	-999000	
			
Mask	Pattern	Result	
1	1010	1009	
10	1010	1000	<-
100	1010	910	
1000	1010	10	<-
10000	1010	-8990	
100000	1010	-98990	
1000000	1010	-998990	
			
Mask	Pattern	Result	
1	1100	1099	
10	1100	1090	
100	1100	1000	<-
1000	1100	100	<-
10000	1100	-8900	
100000	1100	-98900	
1000000	1100	-998900	

*)