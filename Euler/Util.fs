module Util

let isDivisible n d = ((n % d) = 0)
let rec isDivisibleByList n list =
    match list with
    | [] -> true
    | _ -> if (isDivisible n list.Head) then isDivisibleByList n list.Tail else false

