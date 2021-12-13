module Euler6

(*
$RK: Note that formula objects do not sho up properly here
The sum of the squares of the first ten natural numbers is,

The square of the sum of the first ten natural numbers is,

Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is .

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
https://projecteuler.net/problem=6
*)

[<Literal>]
let LIMIT = 100

//[<EntryPoint>]
let main argv =
    let sumOfSquares = [1 .. LIMIT] |> List.map (fun n -> n*n) |> List.sum
    let sum = [1 .. LIMIT] |> List.sum
    let squareOfSum = sum*sum
    let difference = squareOfSum - sumOfSquares
    printfn $"{squareOfSum} - {sumOfSquares} = {difference}"
    0