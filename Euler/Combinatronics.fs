module Combinatronics

open System
    
let rec words alphabet length =
    seq {
        match length with
        | 0 -> yield []
        | n ->
            for letter in alphabet do
                for word in (words alphabet (length-1)) do
                    yield letter::word
    }
