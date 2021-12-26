module Euler31

(*
In the United Kingdom the currency is made up of pound (£) and pence (p). There are eight coins in
general circulation:

1p, 2p, 5p, 10p, 20p, 50p, £1 (100p), and £2 (200p).

It is possible to make £2 in the following way:

1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
How many different ways can £2 be made using any number of coins?
https://projecteuler.net/problem=31
*)
 
//This is something like, "how many solutions are there to this equation:
//    a + 2b + 5c + 10d + 20e + 50f + 100g + 200h = 200

open System

let solve =
    seq{
        yield (0, 0, 0, 0, 0, 0, 0, 1)
        for a in 0..(200/1) do
            let bMax = (200-a)/2
            for b in 0..bMax do
                let cMax = (200 - (a+b*2))/5
                for c in 0..cMax do
                    let dMax = (200 - (a + 2*b + 5*c))/10
                    for d in 0..dMax do
                        let eMax = (200 - (a + 2*b + 5*c + 10*d))/20
                        for e in 0..eMax do
                            let fMax = (200 - (a + 2*b + 5*c + 10*d + 20*e))/50
                            for f in 0..fMax do
                                let gMax = (200 - (a + 2*b + 5*c + 10*d + 20*e + 50*f))/100
                                for g in 0..gMax do
                                        if (a + 2*b + 5*c + 10*d + 20*e + 50*f + 100*g) = 200 then yield (a,b,c,d,e,f,g,0)
    }


//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let solutions = solve
    //printfn "%s" (String.Join('\n', solutions))
    let sum = solutions |> Seq.length
    printfn $"Found {sum} solutions."
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0