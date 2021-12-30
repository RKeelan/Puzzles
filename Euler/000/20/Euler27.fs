module Euler27

(*
Euler discovered the remarkable quadratic formula:

n^2 + n + 41

It turns out that the formula will produce 40 primes for the consecutive integer values 0 <= n <= 39.
However, when n = 40, 40^2 + 40 + 41 is divisible by 41, and certainly when n = 41, 41^2 + 41 + 41
is clearly divisible by 41.

The incredible formula n^2 - 79n + 1601 was discovered, which produces 80 primes for the
consecutive values 0 <= n <= 79 . The product of the coefficients, −79 and 1601, is −126479.

Considering quadratics of the form:

n^2 + an + b, where |a| < 1000 and |b| <= 1000

where |n| is the absolute value of n
e.g., |11| = 11 and |-4| = 4

Find the product of the coefficients, a and b, for the quadratic expression that produces the
maximum number of primes for consecutive values of n, starting with 0.
*)

(*
Debugging
- I tried removing the optimization for b in searchForCoefficients, but it didn't change the results
- I noticed some inconsistency on the constraints on a and b, but it didn't change the results
*)

// Note that the problem states that a < 1000 while b <= 1000. Given that 1000 isn't a prime
// number, and that the formulat evalutes to b when n=0, thtis shouldn't matter. Neverth
let N = 999

let evaluate a b n = n*n + a*n + b

let consecutivePrimes a b =
    if not (Primes.isPrime b) then 0 // If b isn't prime, this will trivially fail when n = 0
    else
        let length = Seq.initInfinite(fun i -> i-1) |> Seq.takeWhile (fun n -> Primes.isPrime (evaluate a b n)) |> Seq.length
        // The length of the sequence will always be one greater than the number of primes found
        length - 1

let searchForCoefficients =
    let primesUnderN = Primes.naiveSieve (N+1)
    seq { for a in [0..N] do
            for b in primesUnderN do
                yield (a, b, (consecutivePrimes a b))
                yield (a, -b, (consecutivePrimes a -b))
                yield (-a, b, (consecutivePrimes -a b))
                yield (-a, -b, (consecutivePrimes -a -b))
            }


//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    //let length_1_4 = Seq.initInfinite(id) |> Seq.takeWhile (fun n -> Prime.isPrime (evaluate 1 4 n)) |> Seq.length
    //printfn $"\"n^2 + n + 4\" yields {length_1_4} primes"

    //let length_1_41 = Seq.initInfinite(id) |> Seq.takeWhile (fun n -> Prime.isPrime (evaluate 1 41 n)) |> Seq.length
    //printfn $"\"n^2 + n + 41\" yields {length_1_41} primes"
    
    //let length_1_79 = Seq.initInfinite(id) |> Seq.takeWhile (fun n -> Prime.isPrime (evaluate -79 1601 n)) |> Seq.length
    //printfn $"\"n^2 - 79n + 1601\" yields {length_1_79} primes"

    let candidates = searchForCoefficients
    //candidates |> Seq.iter (fun c -> printfn "%A" c)
    let (a, b, consecutivePrimes) = candidates |> Seq.maxBy Util.thirdOf3
    printfn $"\"n^2 + {a}n + {b}\" yields {consecutivePrimes} primes"
    printfn $"{a} * {b} = {a*b}" 
    //"n^2 + -61n + 971" yields 71 primes
    //-61 * 971 = -59231
    //Execution time: 1,377 ms.
    // RK 21-Dec-2021: Note that you only get this result if you don't consider negative numbers to
    // be prime
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0