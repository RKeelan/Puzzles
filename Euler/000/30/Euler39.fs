module Euler39

(*
If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are
exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised?
https://projecteuler.net/problem=39
*)

(*
- Looking for (a,b,c) where a+b+c = p and a^2 + b^2 + c^2
- What range of P to look at?
    - I don't think ther's much to gain here
    - I can start at p = (3+4+5), but that's not much of a gain
- But maybe I shouldn't start with p
- If I were to do for i in a..N / for b in i+1..N, I would have to look at N^2 (and also store
results in a map to keep track of which perimeter different tuples were for
    - The inner loop would be from i up to keep from checking (e.g.,) 3,4,5 and 4,3,5
- The other trick is deciding what N should be
    - I guess I could go up to 1/3 of 1000,
        - I know b and c will be larger than a, so if a = 333, then I already know the combined
        perimeter will be over 1000
    - the b side has to go higher than 1/3 of the max perimeter, but I'm not sure how much higher
*)

type Triangle =
    {
        a : int
        b : int
    }
    member this.c = sqrt(double(pown this.a 2) + double(pown this.b 2))
    member this.cInt = int32 this.c
    member this.p = this.a + this.b + int(this.c)
    override this.ToString() = $"{{{this.a}, {this.b}, {this.cInt}, {this.p}}}"

open System

/// Generate all possible integral-length right-triangles with perimeter less than N
let triangles N =
    seq {
    for a in 3..N/3 do
        for b in a .. N/2 do
            let t = {a=a;b=b}
            if Util.isInteger t.c && t.p <= N then yield t
    }

let perimeterSolutions N =
    let mutable map = Map.empty
    for triangle in (triangles N) do
        match map.TryGetValue(triangle.p) with
        | (true, v) -> map <- Map.add triangle.p (v+1) map
        | _ -> map <- Map.add triangle.p 1 map
    map

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let N = 1000
    //let triangles = triangles N
    //printfn "%s" (String.Join('\n', triangles))

    let perimeters = perimeterSolutions N
    printfn "%s" (String.Join("\n", perimeters))
    let max = perimeters |> Map.toArray |> Array.maxBy snd
    printfn $"The perimeter with the most solutions is {fst max} with {snd max} solutions"
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0