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
            if n = 0L then None
            else Some((n % radix), (n/radix)))
        if List.isEmpty initialComponents then BigInt([0L], radix)
        else BigInt(initialComponents, radix)
        
    member self.Radix = radix
    member self.Components = components

    // Object Overrides ---------------------------------------------------------------------------
    
    override this.ToString() =
        match List.rev(components) with
        | [] -> ""
        | head :: [] -> head.ToString()
        | head :: tail -> head.ToString() + String.Join("", tail |> List.map (fun n -> n.ToString(formatString)))

    override self.Equals(otherObj:obj) = 
        match otherObj with
        | :? BigInt as other -> (self.Components = other.Components) && (self.Radix = other.Radix)
        | _ -> false
    
    override self.GetHashCode() =
        let mutable hash = self.Radix.GetHashCode()
        hash <- hash * 23 + self.Components.GetHashCode()
        hash
        
    // Comparison ---------------------------------------------------------------------------------

    interface IComparable with
        member this.CompareTo(otherObj) =
            match otherObj with
            | :? BigInt as other ->
                if this.Radix <> other.Radix then do raise <| ArgumentException("Can't compare instances of different radices")
                let lengthComparison = compare (List.length this.Components) (List.length other.Components)
                if lengthComparison <> 0 then lengthComparison
                else compare (List.rev this.Components) (List.rev other.Components)
            | _ -> raise <| ArgumentException("Can't compare instances of different types")
        
    // Miscellaneous Members ----------------------------------------------------------------------

    member this.sumOfDigits () : int64 =
        this.ToString().ToCharArray() |> Array.fold (fun (acc:int64) (c:char) ->
            acc + Int64.Parse(c.ToString())) 0L

    // Miscellaneous Static Members ---------------------------------------------------------------
    
    static member private trim (bigInt : BigInt) =
        let index = bigInt.Components |> List.tryFindIndexBack (fun n -> n <> 0)
        if index.IsNone then BigInt.zero bigInt.Radix
        else 
            let components = List.take (index.Value + 1) bigInt.Components
            new BigInt(components, bigInt.Radix)
            
    static member zero radix = new BigInt(0, radix)

    static member scalar (bigInt:BigInt) : Option<Int64> =
        try
            Some(bigInt.Components |> List.mapi (fun i n -> n * (pown bigInt.Radix i)) |> List.sum)
        with
            | _ -> None
        
    // Addition -----------------------------------------------------------------------------------

    static member private addRec
        (a : list<int64>)
        (b : list<int64>)
        (radix : int64)
        (carryOver : int64)
        : list<int64> =
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
        new BigInt((BigInt.addRec a.Components b.Components a.Radix 0L), a.Radix)

    static member (+) (a:BigInt, b:int64) : BigInt =
        new BigInt((BigInt.addRec a.Components [b] a.Radix 0L), a.Radix)
        
    // Subtraction --------------------------------------------------------------------------------
   
    static member private subRec
        (a : list<int64>)
        (b : list<int64>)
        (radix : int64)
        (borrow : bool)
        : list<int64> =
        match (a, b) with
        | ([], []) ->
            if borrow then raise(ArgumentException("BigInt does not support negative numbers."))
            else []
        | ([], _) -> raise(ArgumentException("BigInt does not support negative numbers."))
        | ((aHead :: []), []) ->
            let newHead = 
                if borrow then aHead - 1L
                else aHead
            if newHead < 0 then raise(ArgumentException("BigInt does not support negative numbers."))
            elif newHead = 0 then []
            else [newHead]
        | ((aHead :: aTail), []) ->
            let newHead = 
                if borrow then aHead - 1L
                else aHead
            if newHead < 0 then (newHead + radix) :: (BigInt.subRec aTail [] radix true)
            else newHead :: (BigInt.subRec aTail [] radix false)
        | ((aHead :: aTail), (bHead :: bTail)) ->
            let newHead = 
                if borrow then aHead - bHead - 1L
                else aHead - bHead
            if newHead < 0 then (newHead + radix) :: (BigInt.subRec aTail bTail radix true)
            else newHead :: (BigInt.subRec aTail bTail radix false)
            
    static member (-) (a:BigInt, b:BigInt) : BigInt =
        if a.Radix <> b.Radix then do raise(ArgumentException("Both operands need the same radix"))
        let components = BigInt.subRec a.Components b.Components a.Radix false
        BigInt.trim (new BigInt(components, a.Radix))
        
    // Multiplication -----------------------------------------------------------------------------
    
    static member private multiplyScalar
        (multiplicand : list<int64>)
        (multiplier : int64)
        (radix : int64)
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
            newHead :: (BigInt.multiplyScalar tail multiplier radix newCarryOver)
                
    static member (*) (a:int64, b:BigInt) : BigInt =
        let cComponents = BigInt.multiplyScalar b.Components a b.Radix 0L
        new BigInt(cComponents,b.Radix)
                
    static member (*) (a:BigInt, b:int64) : BigInt =
        let cComponents = BigInt.multiplyScalar a.Components b a.Radix 0L
        new BigInt(cComponents,a.Radix)

    static member private multiplyBigInt
        (multiplicand : BigInt)
        (multiplier : list<int64>)
        (acc : BigInt)
        : BigInt =
        match multiplier with
        | [] -> acc
        | head::tail ->
            // This performs multiplication step where you multiply one digit by every digit in the
            // other number
            let acc = acc + (head * multiplicand)
            // This performs the step where, the the next digit is shifted left. (Head is less 
            // significant and tail is more significant.)
            let multiplicand = multiplicand * multiplicand.Radix
            BigInt.multiplyBigInt multiplicand tail acc
            
    static member (*) (a:BigInt, b:BigInt) : BigInt =
        if a.Radix <> b.Radix then do raise(ArgumentException("Both operands need the same radix"))
        BigInt.multiplyBigInt a b.Components (BigInt(0L, a.Radix))

    // Factorial ----------------------------------------------------------------------------------

// Static Functions -------------------------------------------------------------------------------

let rec factorial n : BigInt =
    match n with
    | 0L -> new BigInt(0L, INT_32_RADIX)
    | 1L -> new BigInt(1L, INT_32_RADIX)
    | _ -> (factorial (n - 1L)) * n