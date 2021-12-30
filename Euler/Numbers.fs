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

// TODO Make these generic.
let inline isEven n = isDivisible n 2
let inline isEven64 n = isDivisible n 2L

let inline isOdd n = not (isEven n)
let inline isOdd64 n = not (isEven64 n)

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


let isTriangleNumber t =
    match t with
    | z when z <= 0 -> false
    | _ ->
        // t = (1/2)*n*(n+1)
        // 2*t = n*(n+1)
        // 2*t = n^2 + n
        // 0 = n^2 + n - 2*t
        // 0 = ax^2 + bx + c, with a = b = 1, and c = -2t
        // Using the quadratic formula, and dissalowing negative n,
        // n = (-1 + sqrt(1 - 4*(-2*t)))/2
        let tDouble = double t
        let n = (-1. + sqrt(1. + 8.*tDouble))/2.
        
        // If n is an integer, then t is the nth triangle number
        isInteger n

/// Return the nth triangle number
let triangleNumber n = (n*(n+1))/2
let triangleNumbers = Seq.initInfinite triangleNumber

let isPentagonNumber p =
    match p with
    | z when z <= 0 -> false
    | _ ->
        // p = (n*(3n-1))/3
        // 2*p = n*(3n-1)
        // 2*p = 3n^2 - n
        // 0 = 3n^2 - n - 2*p
        // 0 = ax^2 + bx + c, with a = 3, b = -1, and c = -2p
        // Using the quadratic formula x = (-b +/- sqrt(b^2 - 4ac))/2a, and dissalowing negative n,
        // n = (1 + sqrt(1 - 4*3*(-2p)))/2*3
        // n = 
        let pDouble = double p
        let n = (1. + sqrt(1. + 24.*pDouble))/6.
        
        // If n is an integer, then p is the nth triangle number
        isInteger n

/// Return the nth pentagon number
let pentagonNumber n = (n*(3*n-1))/2


let isHexagonNumber h =
    match h with
    | z when z <= 0 -> false
    | _ ->
        // h = n*(2n-1)
        // h = 2n^2 - n
        // 0 = 2n^2 - n - h
        // 0 = ax^2 + bx + c, with a = 2, b = -1, and c = -h
        // Using the quadratic formula x = (-b +/- sqrt(b^2 - 4ac))/2a, and dissalowing negative n,
        // n = (1 + sqrt(1 - 4*2*(-h)))/2*2
        // n = (1 + sqrt(1 + 8*h))/4
        let hDouble = double h
        let n = (1. + sqrt(1. + 8.*hDouble))/4.
        
        // If n is an integer, then h is the nth triangle number
        isInteger n


let hexagonNumber n = n*(2*n-1)