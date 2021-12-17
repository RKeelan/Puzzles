module File1

(*
Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.

(Note that the image didn't paste properly into this comment)

How many such routes are there through a 20×20 grid?
https://projecteuler.net/problem=15
*)

(*
- A 2x2 grid is a 3x3 lattice of nodes
- More generally, an nxn grid is a (n+1)x(n+1) lattice of nodtes
*)

open System

type point = {x : int; y : int}

let rec latticePaths (grid:Option<int64>[,]) (point:point) =
    let xDim = grid.GetLength(0)
    let yDim = grid.GetLength(1)
    if grid.[point.x,point.y] <> None then grid.[point.x,point.y].Value
    else match point with
            | p when p.x < (xDim - 1) && p.y < (yDim - 1) ->
                let pointBelow = { x = p.x; y = p.y + 1 }
                let pointBeside = { x = p.x + 1; y = p.y }
                let paths = (latticePaths grid pointBelow) + (latticePaths grid pointBeside)
                grid.[point.x,point.y] <- Some(paths)
                printfn $"\t{paths} paths from ({point.x}, {point.y}) to the destination."
                grid.[point.x,point.y].Value

            | p when p.x = xDim && p.y = yDim ->
                // Should never get here, but 0 is the rigth value 
                0L 
            | _ ->
                // at the right or bottom edge, there's only one path to the bottom right corner
                printfn $"\t1 path from ({point.x}, {point.y}) to the destination."
                grid.[point.x,point.y] <- Some(1L)
                grid.[point.x,point.y].Value

let printGrid (grid:Option<int64>[,]) =
    for rowIndex = 0 to (grid.GetLength(0) - 1) do
        for colIndex = 0 to (grid.GetLength(0) - 1) do
            let cell = if grid.[rowIndex, colIndex].IsSome then grid.[rowIndex, colIndex].Value else 0L
            printf $"{cell}\t" 
        printfn ""
            

//[<EntryPoint>]
let main argv =
    let n = 20
    let grid : Option<int64>[,] = Array2D.zeroCreate (n+1) (n+1)
    printfn $"Searching for paths in a {n}x{n} matrix..."
    let origin : point = {x = 0; y = 0}

    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let paths = latticePaths grid origin
    stopWatch.Stop()
    
    printGrid grid
    printfn  $"{paths} paths found in total."
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0