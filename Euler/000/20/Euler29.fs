module Euler29

(*
Consider all integer combinations of ab for 2 ≤ a ≤ 5 and 2 ≤ b ≤ 5:

22=4, 23=8, 24=16, 25=32
32=9, 33=27, 34=81, 35=243
42=16, 43=64, 44=256, 45=1024
52=25, 53=125, 54=625, 55=3125
If they are then placed in numerical order, with any repeats removed, we get the following sequence of 15 distinct terms:

4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125

How many distinct terms are in the sequence generated by ab for 2 ≤ a ≤ 100 and 2 ≤ b ≤ 100?
https://projecteuler.net/problem=29
*)

(*
- I think the strategy here is to figure out how many duplicates there will be
    - In the toy problem, we're iterating [2..5] and [2..5], which yields 4 x 4 = 16 values
    - But there's only distinct 15 terms, because 2^4 = 4^2
    - In the real problem, we're iterating [2..100 and [2..100], which is 99 * 99 = 9801 potential values
- I think an important insight is that 2^2 = 4, and 4 is in the "a" sequence of [2..5]
- a^n * a^m = a^(n+m)
    - 2^4 = 2^2 * 2^2 = 4*4 = 4^2
- Maybe one angle of attack is to determine which of the 'a's are powers of other as
    - Given that a only goes up to 100, we can enumberate the potential duplicatos:
        - 2^2 = 4
        - 2^3 = 8
        - 2^4 = 16
        - 2^5 = 32
        - 2^6 = 64
        - 3^2 = 9
        - 3^3 = 27
        - 3^4 = 81
        - 4^2 = 16
        - 4^3 = 64
        - 5^2 = 25
        - 6^2 = 36
        - 7^2 = 49
        - 8^2 = 64
        - 9^2 = 81
        - 10^2 = 100
    - I'm not exactly sure what to do with this list, though
    - Maybe something like, enumeratoe all the (a,b) pairs, then remove those that can be expressed
    as a combination of any of those other guys
- For 2 and 4, every even power of 4^n is equal to 2^2N
    - More generally, if a^b = x, then for every n, x^(n) = a^(b*n)
*)

open System

// The logic I want to express is:
//  - For every a in A and every b in B,
//      - If 'a' cannot be expressed as a^b, add (B-1) to the accumulator
//      - Otherwise, one add 1 for each a^b that cannot be expressed as a lesser power (...)
//  - Right now, my sticking point is that if I start by iterating over 'a's...
//  - Basically, I want to short-circuit out of searching a^b once it gets higher than the limit,
//  because there are some a^b that are too large to evaluate

// Maybe I need to do a takeWhile

let composites n =
    [for a in [2..n] do
        yield! seq { for b in [2..n] ->
                        (pown a b, a, b) } |> Seq.takeWhile (fun p -> Util.firstOf3 p <= n)]


let countPowers (composites:list<(int*int*int)>) n =
    seq {
        for a in [2..n] do
            // - The question for every a^b is, could this have been produced by earlier a'^b'
            // - Is there an a'^b' such that a'^b' = a
            //      - If so, then (a'^b')^b = a^b, and for every b'*b < N, b is a duplicate
            //          is there an n such that a'^b'
            // - This works, but 4^3 = 8^2, even though no power of 4 is equal to 8
            let candidates = composites |> List.filter (fun (pow, _, _) -> pow = a)
            if List.isEmpty candidates then
                printfn $"\tAll powers of {a} are distinct"
                yield (n - 1)
            else
                // We know there is at least one n^m = a
                let mutable distinct = [2..n]
                for (_, aPrime, bPrime) in candidates do
                    distinct <- distinct |> List.filter (fun b -> b*bPrime > n)
                    printfn $"\tThe distinct powers of {a} after checking {aPrime}^{bPrime} are: {String.Join(',', distinct)}"
                printfn $"\tThe distinct powers of {a} are : {String.Join(',', distinct)}"
    } |> Seq.sum

let powerSet (n:double) : Set<double> =
    let mutable powers = Set.empty
    for a in [2.0..n] do
        for b in [2.0..n] do
            powers <- powers.Add(a**b)
    powers

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let N = 100.0
    //let composites = composites N
    ////printfn "%s" (String.Join("\n", composites))
    //let distinctPowers = countPowers composites N
    //printfn $"There are {distinctPowers} distinct powers for and b less than or equal to {N}"
    
    let distinctPowers = powerSet N
    printfn $"There are {distinctPowers.Count} distinct powers for and b less than or equal to {N}"
    (* I spent hours trying to come up with a clever way to avoid calculating every a^b before it
    ocurred to me that
        - A double actually can represent 100^100
        - I can just add to a set to filter duplicates
    The worst part is that the dumb solution only took 150 ms:

    There are 9183 distinct powers for and b less than or equal to 100
    Execution time: 150 ms.*)
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0