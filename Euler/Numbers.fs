module Numbers

open LanguagePrimitives

// Return the lowest multiple of x that's less than or equal to y, and the factor that produced it
let inline lowestMultiple x y =
    let mutable result = GenericZero
    while result <= y do
        result <- result + x

    // When we break out of the while loop result is the first multiple of x greater than y, so
    // subtract one x
    let multiple = result - x
    let factor = multiple / x
    (multiple, factor) 

// RK 29-Dec-2021: This means reduce the sequence using reduce and the "*" operator
let inline factorial n = 
    match n with
    | m when m = GenericZero -> GenericZero
    | _ -> seq {GenericOne .. n} |> Seq.reduce (*)

let inline summands n = seq { for i in GenericZero .. (n/2) ->  (i,n-i) }

let inline ceilSqrt n = int (ceil (sqrt (float n)))
let inline floorSqrt n = int (floor (sqrt (float n)))

/// Rerturns true is n is divisible by d
let inline isDivisible n d = ((n % d) = GenericZero)

let inline divisorsNaive n =
    seq { for i in GenericOne .. n -> i }
    |> Seq.filter (fun i -> isDivisible n i)
    
// TODO Make this generic.
let inline divisors n =
    match n with
    | 1 -> seq { 1 }
    | 2 -> seq { 1; 2 }
    | _ -> seq { for i in GenericOne .. floorSqrt(n) do
                 if isDivisible n i then
                    let divisor = i
                    let quotient = n/i
                    yield i
                    if divisor <> quotient then yield quotient }

let inline divisorsExSelf n = divisors n |> Seq.filter (fun m -> m <> n)

let inline factors n =
    match n with
    | 1 -> seq { (1,1) }
    | 2 -> seq { (1,2) }
    | _ -> seq { for i in GenericOne .. floorSqrt(n) do
                 if isDivisible n i then yield (i, n/i)}

// TODO Make this generic.
let inline isEven n = isDivisible n 2
let inline isEven64 n = isDivisible n 2L

let inline isDivisibleByAll n list =
    let rec isDivisibleByAll n list =
        match list with
        | [] -> true
        | _ -> if (isDivisible n list.Head) then isDivisibleByAll n list.Tail else false
    isDivisibleByAll n list
    
let inline isDivisibleByAny n list =
    let rec isDivisibleByAny n list =
        match list with
        | [] -> false
        | _ -> if (isDivisible n list.Head) then true else isDivisibleByAny n list.Tail
    isDivisibleByAny n list

let inline product(s : seq<'a> when (^a) : (static member (*) : ^a * ^a -> ^a)) =
    match s with
    | sequence when Seq.isEmpty sequence -> GenericZero
    | _ -> s |> Seq.reduce (fun acc n -> acc*n)
    
let isInteger (d:double) = d%1.=0.