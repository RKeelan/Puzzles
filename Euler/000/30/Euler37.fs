module Euler37

(*
The number 3797 has an interesting property. Being prime itself, it is possible to continuously
remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly
we can work from right to left: 3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to right and right to
left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
https://projecteuler.net/problem=37
*)

(*
1. How will I do the truncation?
    - Truncating from right to left is a matter of dividing by 10
    - Truncating from left to right can be done with a clever modulo
2. What's the bound on the problem?
    - I think we can discard any number containing an even digit, because at some point that digit
    will be the leas-significant one, at which point the number will necessarily be even and not
    prime
    - In fact, we can go further and say that the numbers must start and end with one of 3,5, and 7
    - Maybe we go until we find 11
*)

open System

type direction = Left | Right

let truncate n dir =
    match dir with
    | Left ->
        let length = n.ToString().Length
        if length = 1 then 0
        else 
            let modulous = pown 10 (length - 1)
            n % modulous
    | Right -> n/10

let truncations (n:int) dir = Seq.unfold (fun m ->
    match m with
    | 0 -> None
    | _ -> Some((m, (truncate m dir)))) n
    
let rec isTruncatablePrimeRec numbers dir =
    match numbers with
    | [] -> true
    | (head :: tail) ->
        if Prime.isPrime head then isTruncatablePrimeRec tail dir
        else false


let isTruncatablePrime n =
    let leftCandidates = truncations n Left |> Seq.toList
    if (isTruncatablePrimeRec leftCandidates Left) then
        let rightCandidates = truncations n Right |> Seq.toList
        isTruncatablePrimeRec rightCandidates Right
    else false
        

let truncatablePrimes =
    Seq.initInfinite (fun n -> 13+n*2)
    |> Seq.filter (fun n -> isTruncatablePrime n)
    |> Seq.take 11


//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    //let M = 3797
    //let leftTruncations = truncations M Left
    //printfn $"Left-truncations of {M}:"
    //printfn "%s" (String.Join('\n', leftTruncations))
    
    //let rightTruncations = truncations M Right
    //printfn $"Right-truncations of {M}:"
    //printfn "%s" (String.Join('\n', rightTruncations))

    //printfn $"3797 is a truncatable prime: {isTruncatablePrime 3797}"

    let primes = truncatablePrimes |> Seq.toList
    printfn $"The 11 truncatable primes:"
    printfn "%s" (String.Join('\n', primes))
    let sum = primes |> List.sum
    printfn $"The sum is {sum}"

    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0