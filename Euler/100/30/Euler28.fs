module Euler28

(*
Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:

 (21)  22   23   24  (25)
  20   (7)   8   (9)  10
  19    6   (1)   2   11
  18   (5)   4   (3)  12
 (17)  16   15   14  (13)

It can be verified that the sum of the numbers on the diagonals is 101.

What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
*)

(*
- The naive approach is to make the matrix and calculate the diagonals.
    - A 1001x1001 has a bit over 1 million elements. On a modern machine, quite manageable
- But I think there's a better way.
    - Note this pattern of summands:
        - 1

        - 3
        - 5
        - 7
        - 9

        - 13
        - 17
        - 21
        - 25
    - After the one, there's a block of 4 which are each 2 apart, then a block of 4 which are each
    4 apart
    - I made a 7x7 matrix in excel, and the next set of diagonals were 31, 37, 43, and 49
    - The gap before a block is equal to the gap within that block
    - I don't have a mathematical proof of this, but geometrically, it makes sense
- So, what's the formula?
- A 1001x1001 matrix will have the 1 in the center row / column, plus 500 rows or colunms above,
below, and to either side
    - I think that means there will be 500 blocks
        - a 3x3 matrix has 1 block
        - a 5x5 matrix has 2 blocks
        - a 7x7 matrix has 3 blocks
        - An NxN matrix will have (N-1)/2 blocks
        - 1 1001x1001 matrix will have (1001-1)/2 = 500 blocks
    - The Nth block will have 2N between each term
        - The 0th block has 2 between the terms
        - The 1st block has 4 between the terms
        - The 2nd block has 6 between the terms
        - The Nth block has 2N+2 between the terms
- Let's say I want to make a sequence. How many terms should it have?
    - a 1x1 matrix has 1 term
    - a 3x3 matrix has 5 terms
    - a 5x5 matrix has 9 terms
    - a 7x7 matrix has 13 terms
    - This is #blocks * 4 + 1
    - But a better way of looking at it is 
*)

open System

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    // The size (number of rows and columns) of the matrix. Note that this calculation will only
    // work for square matrices with an odd number of rows
    let N = 1001

    // Intuitively, each diagonal extends out from the center into either the top-left, top-right
    // (etc) corner. Since it's from the center but excluding the center, it's half of N-1
    let termsInADiagonal = (N-1)/2

    // The total length of the sequence is the number of terms in each diagonal * the number of
    // diagonals, plus one for the center
    let sequenceLength = termsInADiagonal*4 + 1
    let increments = Seq.init (sequenceLength - 1) (fun n -> 2*(1+(n/4)))
    //printfn "Increments: %s" (String.Join(" ", increments))
    let terms = Seq.scan (fun acc increment -> (acc + increment)) 1 increments
    //printfn "Terms: %s" (String.Join(" ", terms))
    let sum = Seq.sum terms
    printfn $"The sum of the diagonals is {sum}"
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0