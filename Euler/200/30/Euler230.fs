module Euler30

(*
For any two strings of digits, A and B, we define F-A,B to be the sequence (A,B,AB,BAB,ABBAB,...)
in which each term is the concatenation of the previous two.

Further, we define D-A,B(n) to be the nth digit in the first term of F-A,B that contains at least
n digits.

Example:

Let A=1415926535, B=8979323846. We wish to find DA,B(35), say.

The first few terms of FA,B are:
1415926535
8979323846
1415926535 8979323846
8979323846 1415926535 8979323846
1415926535 8979323846 8979323846 1415(9)26535 8979323846

Then DA,B(35) is the 35th digit in the fifth term, which is 9.

Now we use for A the first 100 digits of π behind the decimal point:

14159265358979323846264338327950288419716939937510
58209749445923078164062862089986280348253421170679

and for B the next hundred digits:

82148086513282306647093844609550582231725359408128
48111745028410270193852110555964462294895493038196 .

Find ∑n = 0,1,...,17   10n× DA,B((127+19n)×7n) .
*)

(*
- The naive approach to this question requires making a character array that is at least
(127+(19*17)*7^17 bytes long. This is, it suffices to say, more memory than any reasonable PC has
to spare
- I got a bunch of the way through thsi before I realized that it was problem #230, not #30, and
I don't want to solve it until I get to it
- The task is to find the first term which is long enough to contain an nth digit.
- My approach is to calculate how long that term would be (which is relatively easy), then use that
length to search backards from the end of the term
- My next task to get revF working, which is the function that builds the sequence backwards
*)

open System

let a = 9542736314269218347UL

let revF (a:string) (b:string) : seq<string> =
    seq {
        yield a
        yield b
        yield! Seq.unfold (fun (f1,f2) -> Some((f2+f1,(f2, f2+f1)))) (a, b)
    }

let lengthF (a:uint64) (b:uint64) : seq<uint64> =
    seq {
        yield a
        yield b
        yield! Seq.unfold (fun (f1,f2) -> Some((f1+f2,(f2, f1+f2)))) (a, b)
    }

let termLengthD (a:uint64) (b:uint64) (n:uint64) = (lengthF a b) |> Seq.find (fun t -> t >= n)
let searchBackwards (a:string) (b:string) n =
    let s = Seq.find (fun (s:string) -> s.Length >= n) (revF a b)
    printfn $"Looking in {s}"
    Int64.Parse(s.Substring(s.Length-n))

let sevenExp (n:int64) : int64 = pown 7L (int32 n)

[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let a = "1415926535"
    let b = "8979323846"
    let termLengths = lengthF (uint64 a.Length) (uint64 b.Length)
    let fifthLength = Seq.take 5 termLengths
    printfn "%s" (String.Join("\n", fifthLength))
    let d = 35UL
    let termLength = termLengthD 10UL 10UL d
    printfn $"The first term to contain a digit #{d} is {termLength} characters"
    let digit = searchBackwards a b (int32 (termLength - d + 1UL))
    printfn $"The digit at position {d} is {digit}"
    // Now we can search backwards from the end..
    // - The 35th digit counting from the front is the 14th digit from the end of a 50 digit sequence
    // - Or, more generally, length - index - 1

    //let N = 17L
    //let a = "1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679"
    //let b = "8214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196"
    //let index = (F a b) |> Seq.findIndex (fun (s:string) -> s.ToCharArray().LongLength >= ((127L+(19L*N))*(sevenExp N))) 
    //let terms = (F a b) |> Seq.take index |> Seq.toList
    //let sum = [0L..N] |> Seq.map (fun n -> (pown 10L (int32 n)) * D terms a b ((127L+(19L*n))*(sevenExp n))) |> Seq.sum
    //printfn $"Sum is {sum}"
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0