module Strings

open System
open Numbers

let hasDuplicates (s:string) : bool =
    let set = s.ToCharArray() |> Set.ofSeq
    set.Count < s.Length

let rotate (s:string) = s.Substring(1) + s.[0].ToString()

let rotations s = seq {
    yield s
    yield! rotate s |> Seq.unfold (fun str ->
        match str with
        | original when original = s -> None
        | _ -> Some((str, rotate str)))
    }

let isPalindrome (s:string) =
    let frontHalf = s.Substring(0, s.Length/2)
    let backHalfStart = if isEven s.Length then s.Length/2 else s.Length/2 + 1
    let backHalf = new string(s.Substring(backHalfStart) |> Seq.rev |> Seq.toArray)
    frontHalf = backHalf

let isPandigital (s:string) d = not(s.Contains('0')) && (s.Length = d) && (not (hasDuplicates s))

// Permutations -----------------------------------------------------------------------------------

let lexicographicPermutations (s:string) : list<string> =
    let chars = s.ToCharArray()
    Array.Sort chars

    let mutable result = [new string(chars)]
    let mutable permuting = true
    while permuting do
        let mutable k = -1
        for i in 0..chars.Length-2 do
            if chars.[i] < chars.[i+1] then do k <- i
            
        if k = -1 then do permuting <- false
        else
            let mutable l = -1
            for i in (k+1)..(chars.Length-1) do
                if chars.[k] < chars.[i] then do l <- i
            let c = chars.[k]
            chars.[k] <- chars.[l]
            chars.[l] <- c
            Array.Reverse(chars, (k+1), (chars.Length - 1 - k))
            result <- (new string(chars)) :: result
    result |> List.rev 