module BigInt

open System

// The biggest power-of-ten radix that will fit insisde an int32
[<Literal>]
let INT_32_RADIX = 1000000000L

// The biggest power-of-ten radix that will fit insisde an int64
[<Literal>]
let INT_64_RADIX = 1000000000000000000L

type BigInt (componentsIn : list<int64>, radixIn : int64) =
    let radix = radixIn
    let formatString = new string((radix - 1L).ToString().ToCharArray() |> Array.map (fun c -> '0'))
    let components = componentsIn

    new(initialValue : int64, radix : int64) =
        let initialComponents:list<int64> = initialValue |> List.unfold (fun n ->
            if n = 0L then
                None
            else Some((n % radix), (n/radix)))
        BigInt(initialComponents, radix)

    override this.ToString() =
        match List.rev(components) with
        | [] -> ""
        | head :: [] -> head.ToString()
        | head :: tail -> head.ToString() + String.Join("", tail |> List.map (fun n -> n.ToString(formatString)))

    member private this.multiplyInternal
        (multiplicand : list<int64>)
        (multiplier : int64)
        (carryOver : int64)
        : list<int64> =
        match multiplicand with
        // The least-significant element is the head of the list, so if we get to the end, return
        // either nothing or the carry-over
        | [] -> if carryOver = 0L then [] else [carryOver]
        | head::tail ->
            let newHead =
                if (head * multiplier) >= radix then
                    (head * multiplier + carryOver) % radix
                else (head * multiplier + carryOver)
            let newCarryOver = (head * multiplier) / radix
            newHead :: (this.multiplyInternal tail multiplier newCarryOver)

    member this.multiply (multiplier : int64) : BigInt =
        new BigInt((this.multiplyInternal components multiplier 0L), radix)