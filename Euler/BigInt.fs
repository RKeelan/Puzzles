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
        
    member self.Radix = radix
    member self.Components = components

    override this.ToString() =
        match List.rev(components) with
        | [] -> ""
        | head :: [] -> head.ToString()
        | head :: tail -> head.ToString() + String.Join("", tail |> List.map (fun n -> n.ToString(formatString)))

    member private this.multiplyRec
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
            newHead :: (this.multiplyRec tail multiplier newCarryOver)

    member this.multiply (multiplier : int64) : BigInt =
        new BigInt((this.multiplyRec components multiplier 0L), radix)

    member this.sumOfDigits () : int64 =
        this.ToString().ToCharArray() |> Array.fold (fun (acc:int64) (c:char) ->
            acc + Int64.Parse(c.ToString())) 0L
            
    // RK 20-Dec-2021: It's a bit sloppy to have addition by static while multiplication is
    // instanced, but I'm not sure which approach is more correct, so I'm going with whatever's
    // convenient for the task at haand
    static member private addRec
        (a : list<int64>)
        (b : list<int64>)
        (radix : int64)
        (carryOver : int64) : list<int64> =
        match (a, b) with
        | ([], []) ->
            if carryOver = 0L then []
            elif carryOver < radix then [carryOver]
            else carryOver :: (BigInt.addRec a b radix (carryOver/radix))
        | ([], (bHead :: bTail)) ->
            let newHead = 
                if (bHead + carryOver) >= radix then
                   (bHead + carryOver) % radix
                else (bHead + carryOver)
            let newCarryOver = (bHead + carryOver) / radix
            newHead :: (BigInt.addRec [] bTail radix newCarryOver)
        | ((aHead :: aTail), []) -> 
            let newHead = 
                if (aHead + carryOver) >= radix then
                   (aHead + carryOver) % radix
                else (aHead + carryOver)
            let newCarryOver = (aHead + carryOver) / radix
            newHead :: (BigInt.addRec [] aTail radix newCarryOver)
        | ((aHead :: aTail), (bHead :: bTail)) ->
            let newHead = 
                if (aHead + bHead + carryOver) >= radix then
                   (aHead + bHead + carryOver) % radix
                else (aHead + bHead + carryOver)
            let newCarryOver = (aHead + bHead + carryOver) / radix
            newHead :: (BigInt.addRec aTail bTail radix newCarryOver)


    static member (+) (a:BigInt, b:BigInt) : BigInt =
        if a.Radix <> b.Radix then do raise(ArgumentException("Both operands need the same radix"))

        let cComponents = BigInt.addRec a.Components b.Components a.Radix 0L

        new BigInt(cComponents,a.Radix)

// Static Functions -------------------------------------------------------------------------------

let rec factorial n : BigInt =
    match n with
    | 0L -> new BigInt(0L, INT_32_RADIX)
    | 1L -> new BigInt(1L, INT_32_RADIX)
    | _ -> (factorial (n - 1L)).multiply(n)