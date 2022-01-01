module Euler50

(*
The prime 41, can be written as the sum of six consecutive primes:

41 = 2 + 3 + 5 + 7 + 11 + 13

This is the longest sum of consecutive primes that adds to a prime below one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms,
and is equal to 953.

Which prime, below one-million, can be written as the sum of the most consecutive primes?
https://projecteuler.net/problem=50
*)

(*
- Using a naive prime sieve, even just listing primes under 1,000,000 took over a minute, and
a couple GB of memory
    - And, to be clear, I aborted after a minute. I don't know how much longer it would have taken
- Using my prime generator, I was able to list out the 78,498 primes under 1,000,000 in just over 12s
- The most obvious thing is to somehow generate every possible prime chain for every prime under a
million
    - For this I guess I'd add every consecutive prime starting from 2 up to p until the sum either
    equaled p or was greater than it.
        - I think that if I start from 2, the first chain I find that equals p is necessarily the
        longest
            - Let's say I have a chain from n to m which sums to p
            - Any subsequent chain will be replacing n+xs for m-ys, and except in rare two-term
            - chans, the numbers near m will be quite a bit larger than the numbers near n
    - I guess I'd record the long chain for each prime, then find the largest at the end
- Is there any way to improve on this?
    - I'd probably find the biggest chain faster if I searched backwards, but would I know I had
    the longest chain until I examined all (or a bunch of) the smaller primes?
- I could keep track of the largest chain I've found so far, and try to short-ciruit chain searches
that won't produce chains longer than the one I've already found
    - Unfortunately, There are 168 primes just under 1,000, but the longest chain is 21, so that
    alone isn't likely to save me any time
- Using the approach described above, I was unable to solve the problem in 3 minutes (at which time
I aborted
    - A a beanchmark for optimizing, I ran the algorithm on primes under 100,000
        - 9,592 primes under 100,000
        - Longest chain is 183 terms summing to 92,951
        - Ran in about 11s
    - Limiting i to "lesserPrimes.Length/2" at line 54 shaved off about 1s
    - Removing the filtering at line 52 also brought the runtime down to about 10s
    - Limiting the for loop on line 56 to be the length of the array divided by the current longest
    chain, while also searching backwards
        - If the longest chain is 2, then we'll need at lest 3 terms to surpass it, but once we get
        to the halfway point of the array, each term will be roughly 1/2 of p, and a sequence of
        two terms will already be equal to p
        - Similarly, if the chain is 3, then once we get a third of the way through the array, it
        will be too late to find a longer chain
        - This brought runtime on 100,000 primes to 4.5s
        - Results for 1,000,000:
            There are 78,498 primes under 1,000,000
            The length of the longest chain under 1,000,000 is 543 terms.
            Execution time: 268,660 ms.
            (But, ironically, I need the prime, not the chain length, to answer)
        - After switching from foldBack to fold:
            There are 78,498 primes under 1,000,000
            The length of the longest chain under 1,000,000 are 543 terms summing to 999983
            Execution time: 263,019 ms.
        - But the answer is wrong!
        - Going back to List.map, but always diving lesserPrimes.Length by 500 based on the
        previous results I get:
            There are 78,498 primes under 1,000,000
            The length of the longest chain under 1,000,000 are 543 terms summing to 997651
            Execution time: 254,695 ms.
        - 997651 is the right answer, which makes me think there was a bug in my use of fold
- I feel bad about getting this answer with a runtime over one might (even with a machine 10 years
mored advanced than the ones that would have been used in the early 2000s when these questions were
written, but not so bad that I'm going to optimize my solution further
    - I think the next step would be to analyze what exactly the performance issues are. Is it
    memory allocations? Is findSumChainLength still too slow?
    - I don't care to dig in that way for a problem that I mostly solved in a mostly reasonable
    amount of time
*)

open System
open Primes

let findSumChainLength p primes =
//let findSumChainLength currentMax p primes =
    let lesserPrimes = primes |> List.filter (fun n -> n < p) |> List.toArray
    let forLimit = lesserPrimes.Length / 500
    seq {
        for i = 0 to forLimit do
            let mutable sum = 0
            let mutable j = i
            while sum < p && j < lesserPrimes.Length do
                sum <- sum + lesserPrimes.[j]
                j <- j + 1
            if sum = p then yield (j-i)
    } |> Seq.tryHead

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let N = 1000000
    let primes = Primes.primes |> Seq.takeWhile (fun p -> p < N) |> Seq.toList
    let (prime, chainLength) = primes |> List.map (fun p -> (p, (findSumChainLength p primes))) |> List.maxBy snd
    //let (prime, chainLength) = (primes, (0, Some(1))) ||> List.foldBack (fun p (_, len) -> (p, (max len (findSumChainLength len.Value p primes))))
    //let (prime, chainLength) = ((0, Some(1)), primes) ||> List.fold (fun (_, len) p -> (p, (max len (findSumChainLength len.Value p primes))))
    printfn $"There are {primes.Length:N0} primes under {N:N0}"
    printfn $"The length of the longest chain under {N:N0} are {chainLength.Value} terms summing to {prime}"
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0