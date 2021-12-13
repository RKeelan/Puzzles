// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Euler1

[<EntryPoint>]
let main argv =
    let number = sumOfMultiplesBelow 1000
    printf "%d" number
    0 // return an integer exit code