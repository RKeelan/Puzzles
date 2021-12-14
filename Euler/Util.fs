module Util

let ceilSqrt n = int (ceil (sqrt (float n)))
let floorSqrt n = int (floor (sqrt (float n)))

let isDivisible n d = ((n % d) = 0)

let rec isDivisibleByAll n list =
    match list with
    | [] -> true
    | _ -> if (isDivisible n list.Head) then isDivisibleByAll n list.Tail else false
    
let rec isDivisibleByAny n list =
    match list with
    | [] -> false
    | _ -> if (isDivisible n list.Head) then true else  isDivisibleByAny n list.Tail

let isPrime p =
    match p with
    | 1 | 4 | 6 | 8 | 10 -> false
    | 2 | 3 | 5 | 7 | 11 -> true
    | _ ->
        let divisors = [2 .. (ceilSqrt p)]
        //printfn "%A" divisors
        not (isDivisibleByAny p divisors)

let rec exceptSorted a b =
    match (a, b) with
    | [], x | x, [] -> x
    | x::xs, y::ys ->
        if x < y then x :: exceptSorted xs (y::ys)
        elif x > y then y :: exceptSorted (x::xs) ys
        else exceptSorted xs ys