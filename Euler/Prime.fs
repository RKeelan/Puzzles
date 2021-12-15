module Prime

open System.Linq

let isPrime p =
    match p with
    | 1 | 4 | 6 | 8 | 10 -> false
    | 2 | 3 | 5 | 7 | 11 -> true
    | _ ->
        let divisors = [2 .. (Util.ceilSqrt p)]
        //printfn "%A" divisors
        not (Util.isDivisibleByAny p divisors)

let nextPrime start =
    // RK 14-Dec-2021: Use the match to handle various corner cases
    match start with
    | i when i < 2 -> 2
    | 2 -> 3
    | _ ->
        // RK 14-Dec-2021: This is a bit controverial, but I feel like calling "nextPrime" on a
        // prime number should return the next prime after that number, not that number
        let firstNonPrime = if isPrime start then (start + 1) else start
        let sequenceStart = if Util.isEven firstNonPrime then (firstNonPrime + 1) else firstNonPrime
        let candidates = Seq.initInfinite (fun n -> n*2 + sequenceStart)
        Seq.head (candidates |> Seq.filter (fun n -> isPrime n))

let rkPrimes =
    2 // First prime
    |> Seq.unfold (fun state -> Some(state, nextPrime state))
    

let rec nthPrime n = Seq.head(rkPrimes.Skip(n - 1))

// Stack Overflow ---------------------------------------------------------------------------------
// From https://stackoverflow.com/questions/4629734/the-sieve-of-eratosthenes-in-f

let rec sieve list =
    match list with
    | head::tail -> head :: (sieve <| List.filter (fun x -> x % head <> 0) tail)
    | [] -> []

let unfaithfulPrimeSieve n = sieve [2 .. n]

type GenericHeap<'T when 'T : comparison>(defaultValue : 'T) =
    let mutable capacity = 1
    let mutable values = Array.create capacity defaultValue
    let mutable size = 0

    let swap i n =
        let temp = values.[i]
        values.[i] <- values.[n]
        values.[n] <- temp

    let rec rollUp i =
        if i > 0 then
            let parent = (i - 1) / 2
            if values.[i] < values.[parent] then
                swap i parent
                rollUp parent

    let rec rollDown i =
        let left, right = 2 * i + 1, 2 * i + 2

        if right < size then
            if values.[left] < values.[i] then
                if values.[left] < values.[right] then
                    swap left i
                    rollDown left
                else
                    swap right i
                    rollDown right
            elif values.[right] < values.[i] then
                swap right i
                rollDown right
        elif left < size then
            if values.[left] < values.[i] then
                swap left i

    member this.insert (value : 'T) =
        if size = capacity then
            capacity <- capacity * 2
            let newValues = Array.zeroCreate capacity
            for i in 0 .. size - 1 do
                newValues.[i] <- values.[i]
            values <- newValues

        values.[size] <- value
        size <- size + 1
        rollUp (size - 1)

    member this.delete () =
        values.[0] <- values.[size]
        size <- size - 1
        rollDown 0

    member this.deleteInsert (value : 'T) =
        values.[0] <- value
        rollDown 0

    member this.min () =
        values.[0]

    static member Insert (value : 'T) (heap : GenericHeap<'T>) =
        heap.insert value
        heap    

    static member DeleteInsert (value : 'T) (heap : GenericHeap<'T>) =
        heap.deleteInsert value
        heap    

    static member Min (heap : GenericHeap<'T>) =
        heap.min()

type Heap = GenericHeap<int64 * int * int64>

let wheelData = [|2L;4L;2L;4L;6L;2L;6L;4L;2L;4L;6L;6L;2L;6L;4L;2L;6L;4L;6L;8L;4L;2L;4L;2L;4L;8L;6L;4L;6L;2L;4L;6L;2L;6L;6L;4L;2L;4L;6L;2L;6L;4L;2L;4L;2L;10L;2L;10L|]

let soPrimes() = 
    // the priority queue functions
    let insert = Heap.Insert
    let findMin = Heap.Min
    let insertDeleteMin = Heap.DeleteInsert

    // increments iterator
    let wheel (composite, n, prime) =
        composite + wheelData.[n % 48] * prime, n + 1, prime

    let insertPrime prime n table =
        insert (prime * prime, n, prime) table

    let rec adjust x (table : Heap) =
        let composite, n, prime = findMin table

        if composite <= x then 
            table 
            |> insertDeleteMin (wheel (composite, n, prime))
            |> adjust x
        else
            table

    let rec sieve iterator table =
        seq {
            let x, n, _ = iterator
            let composite, _, _ = findMin table

            if composite <= x then
                yield! sieve (wheel iterator) (adjust x table)
            else
                if x = 13L then
                    yield! [2L; 3L; 5L; 7L; 11L]

                yield x
                yield! sieve (wheel iterator) (insertPrime x n table)
        }

    sieve (13L, 1, 1L) (insertPrime 11L 0 (Heap(0L, 0, 0L)))

let rec soPrimes2() : seq<int64 * int> = 
    // the priority queue functions
    let insert = Heap.Insert
    let findMin = Heap.Min
    let insertDeleteMin = Heap.DeleteInsert

    // increments iterator
    let wheel (composite, n, prime) =
        composite + wheelData.[n % 48] * prime, n + 1, prime

    let insertPrime enumerator composite table =
        // lazy initialize the enumerator
        let enumerator =
            if enumerator = null then
                let enumerator = soPrimes2().GetEnumerator()
                enumerator.MoveNext() |> ignore
                // skip primes that are a part of the wheel
                while fst enumerator.Current < 11L do
                    enumerator.MoveNext() |> ignore
                enumerator
            else
                enumerator

        let prime = fst enumerator.Current
        // Wait to insert primes until their square is less than the tables current min
        if prime * prime < composite then
            enumerator.MoveNext() |> ignore
            let prime, n = enumerator.Current
            enumerator, insert (prime * prime, n, prime) table
        else
            enumerator, table

    let rec adjust x table =
        let composite, n, prime = findMin table

        if composite <= x then 
            table 
            |> insertDeleteMin (wheel (composite, n, prime))
            |> adjust x
        else
            table

    let rec sieve iterator (enumerator, table) = 
        seq {
            let x, n, _ = iterator
            let composite, _, _ = findMin table

            if composite <= x then
                yield! sieve (wheel iterator) (enumerator, adjust x table)
            else
                if x = 13L then
                    yield! [2L, 0; 3L, 0; 5L, 0; 7L, 0; 11L, 0]

                yield x, n
                yield! sieve (wheel iterator) (insertPrime enumerator composite table)
        }

    sieve (13L, 1, 1L) (null, insert (11L * 11L, 0, 11L) (Heap(0L, 0, 0L)))


let mutable i = 0

//let compare a b =
//    i <- i + 1
//    if a = b then
//        true
//    else
//        printfn "%A %A %A" a b i
//        false

//Seq.forall2 compare (Seq.take 50000 (soPrimes())) (Seq.take 50000 (soPrimes2() |> Seq.map fst))
//|> printfn "%A"

//soPrimes2()
//|> Seq.map fst
//|> Seq.take 10
//|> Seq.toArray
//|> printfn "%A"

//soPrimes2()
//|> Seq.map fst
//|> Seq.skip 999999
//|> Seq.take 10
//|> Seq.toArray
//|> printfn "%A"

//System.Console.ReadLine() |> ignore