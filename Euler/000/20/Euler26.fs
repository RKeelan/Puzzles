module Euler26

(*
A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:

1/2	= 	0.5
1/3	= 	0.(3)
1/4	= 	0.25
1/5	= 	0.2
1/6	= 	0.1(6)
1/7	= 	0.(142857)
1/8	= 	0.125
1/9	= 	0.(1)
1/10	= 	0.1
Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
https://projecteuler.net/problem=26
*)

(*
- I think I'll probably need to implement my own decimalization algorithm
- I'll also need to look up how recurring cycles are detected
*)

open System
open Util

let rec divideRec numerator denominator previousNumerators : list<int> =
    if  denominator > numerator then 0 :: (divideRec (numerator*10) denominator previousNumerators)
    else
        let (multiple, factor) = Util.lowestMultiple denominator numerator 
        let remainder = numerator - multiple
        if remainder = 0 then
            //printfn "\tTerminating decimal detected"
            [factor]
        else
            let newNumerator = (remainder*10)
            if previousNumerators |> Set.contains newNumerator then
                //printfn "\tRepeating decimal detected"
                [factor]
            else
                factor :: divideRec newNumerator denominator (previousNumerators.Add(newNumerator))

let divide numerator denominator =
    if denominator = 0 then raise(DivideByZeroException())
    else
        let quotient = divideRec numerator denominator Set.empty
        String.Join("", quotient)
        // RK 21-Dec-2021: This is some code I wrote to try and put the decimal point in the right
        // place which didn't work.
        // Instead of looking at just the numerator, I might try looking at the ratio of numerator
        // to denominator.
        //let decimalPosition = numerator.ToString().Length
        //let wholeNumber = List.take decimalPosition quotient
        //let fraction = List.skip decimalPosition quotient
        //if (List.isEmpty fraction) then String.Join("", wholeNumber)
        //else String.Join("", wholeNumber) + "." + String.Join("", fraction)

let divideAndPrint n d =
    let q = divide n d
    printfn $"{n}/{d} = {q}"

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    //divideAndPrint 9 3
    //divideAndPrint 75 15
    //divideAndPrint 116 4
    //divideAndPrint 1 2
    //divideAndPrint 1 3
    //divideAndPrint 1 4
    //divideAndPrint 1 5
    //divideAndPrint 1 6
    //divideAndPrint 1 7
    //divideAndPrint 1 8
    //divideAndPrint 1 9
    //divideAndPrint 1 11
    //divideAndPrint 1 13

    let quotients = seq { for i in 1 .. 999 -> (i, divide 1 i)}
    let longestCycle = quotients |> Seq.maxBy (fun q -> (snd q).TrimStart('0').Length)
    // It's super janky, but I'm going to guess that the longest faction, excluding leading zeros,
    // is also the longest cycle
    printfn "The longest cycle is %A" longestCycle
    // RK 21-Dec-2021: Lol--
    // The longest cycle is (983, "00010172939979654120040691759918616480162767039674465920651068158697863682604272634791454730417090539165818921668362156663275686673448626653102746693794506612410986775178026449643947100712105798575788402848423194303153611393692777212614445574771108850457782299084435401831129196337741607324516785350966429298067141403865717192268565615462868769074262461851475076297049847405900305188199389623601220752797558494404883011190233977619532044760935910478128179043743641912512716174974567650050864699898270600203458799593082400813835198372329603255340793489318413021363173957273652085452695829094608341810783316378433367243133265513733468972533062054933875890132248219735503560528992878942014242115971515768056968463886063072227873855544252288911495422177009155645981688708036622583926754832146490335707019328585961342828077314343845371312309257375381485249237029501525940996948118006103763987792472024415055951169888097660223804679552390640895218718209562563580874872838250254323499491353001")
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0