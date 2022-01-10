module Euler54

(*
In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way:

High Card: Highest value card.
One Pair: Two cards of the same value.
Two Pairs: Two different pairs.
Three of a Kind: Three cards of the same value.
Straight: All cards are consecutive values.
Flush: All cards of the same suit.
Full House: Three of a kind and a pair.
Four of a Kind: Four cards of the same value.
Straight Flush: All cards are consecutive values of same suit.
Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
The cards are valued in the order:
2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

If two players have the same ranked hands then the rank made up of the highest value wins; for
example, a pair of eights beats a pair of fives (see example 1 below). But if two ranks tie, for
example, both players have a pair of queens, then highest cards in each hand are compared (see
example 4 below); if the highest cards tie then the next highest cards are compared, and so on.

Consider the following five hands dealt to two players:

Hand	 	Player 1	 	Player 2	 	Winner
1	 	5H 5C 6S 7S KD
Pair of Fives
 	2C 3S 8S 8D TD
Pair of Eights
 	Player 2
2	 	5D 8C 9S JS AC
Highest card Ace
 	2C 5C 7D 8S QH
Highest card Queen
 	Player 1
3	 	2D 9C AS AH AC
Three Aces
 	3D 6D 7D TD QD
Flush with Diamonds
 	Player 2
4	 	4D 6S 9H QH QC
Pair of Queens
Highest card Nine
 	3D 6D 7H QD QS
Pair of Queens
Highest card Seven
 	Player 1
5	 	2H 2D 4C 4D 4S
Full House
With Three Fours
 	3C 3D 3S 9S 9D
Full House
with Three Threes
 	Player 1
The file, poker.txt, contains one-thousand random hands dealt to two players. Each line of the file
contains ten cards (separated by a single space): the first five are Player 1's cards and the last
five are Player 2's cards. You can assume that all hands are valid (no invalid characters or
repeated cards), each player's hand is in no specific order, and in each hand there is a clear
winner.

How many hands does Player 1 win?
*)

(*
- So comparing a pair of cards is easy, but how to I rank hands
- My first thought was to implemenet the domain logic of which hands are best
- It occurs to me, now that the hands themselves are ranked based on how likely they are, so I
could actually rank them mathematically
    - But I think that's like taking the easy part of problem and turning it into a much more
    difficult problem
*)

open System
open System.IO

type Suit =
    | Hearts
    | Diamonds
    | Clubs
    | Spades
    
let getSuitString suit =
    match suit with
    | Hearts -> "H"
    | Diamonds -> "D"
    | Clubs -> "C"
    | Spades -> "S"

type CardRank =
    | Two = 2
    | Three = 3
    | Four = 4
    | Five = 5
    | Six = 6
    | Seven = 7
    | Eight = 8
    | Nine = 9
    | Ten = 10
    | Jack = 11
    | Queen = 12
    | King = 13
    | Ace = 14
    
let getCardRankString cardRank =
    match cardRank with
    | CardRank.Two -> "2"
    | CardRank.Three -> "3"
    | CardRank.Four -> "4"
    | CardRank.Five -> "5"
    | CardRank.Six -> "6"
    | CardRank.Seven -> "7"
    | CardRank.Eight -> "8"
    | CardRank.Nine -> "9"
    | CardRank.Ten -> "T"
    | CardRank.Jack -> "J"
    | CardRank.Queen -> "Q"
    | CardRank.King -> "K"
    | CardRank.Ace -> "A"
    | _ -> raise(ArgumentException($"Unexpected card rank: {cardRank}"))

type HandRank =
    | HighCard
    | OnePair
    | TwoPair
    | ThreeOfAKind
    | Straight
    | Flush
    | FullHouse
    | FourOfAKind
    | StraightFlush

[<CustomEquality>]
[<CustomComparison>]
type Card = {
    Suit : Suit
    Rank : CardRank
} with
    override this.ToString() = (getCardRankString this.Rank)+(getSuitString this.Suit)
    override this.Equals(otherObj) =
        match otherObj with
        | :? Card as other -> this.Suit = other.Suit && this.Rank = other.Rank
        | _ -> false
    override this.GetHashCode() =
        let mutable hash = this.Suit.GetHashCode()
        hash <- hash * 23 + this.Rank.GetHashCode()
        hash
    interface IComparable with
        member this.CompareTo(otherObj) =
            match otherObj with
            | :? Card as other -> compare this.Rank other.Rank
            | _ -> raise <| ArgumentException("Can't compare instances of different types")
        
let cardFromString (cardStr:String) =
    let rank = match cardStr[0] with
                | '2' -> CardRank.Two
                | '3' -> CardRank.Three
                | '4' -> CardRank.Four
                | '5' -> CardRank.Five
                | '6' -> CardRank.Six
                | '7' -> CardRank.Seven
                | '8' -> CardRank.Eight
                | '9' -> CardRank.Nine
                | 'T' -> CardRank.Ten
                | 'J' -> CardRank.Jack
                | 'Q' -> CardRank.Queen
                | 'K' -> CardRank.King
                | 'A' -> CardRank.Ace
                | _ -> raise(ArgumentException($"Unexpected card rank: {cardStr[0]}"))
    let suit = match cardStr[1] with
                | 'H' -> Hearts
                | 'D' -> Diamonds
                | 'C' -> Clubs
                | 'S' -> Spades
                | _ -> raise(ArgumentException($"Unexpected suit: {cardStr[1]}"))
    {Suit = suit; Rank = rank}

#nowarn "343"
type Hand(cardsIn) =
    
    let cards = cardsIn |> Array.sortDescending
    let cardRankCounts = cards |> Array.countBy (fun c -> c.Rank) |> Array.sortByDescending snd

    new(handString:string) =
        let cards = handString.Split([|' ';'|'|]) |> Array.map cardFromString
        new Hand(cards)

    member val Cards = cards with get
    member val CardRankCounts = cardRankCounts with get
    member this.Rank =
        if this.IsStraightFlush() then StraightFlush
        elif this.IsFourOfAKind() then FourOfAKind
        elif this.IsFullHouse() then FullHouse
        elif this.IsFlush() then Flush
        elif this.IsStraight() then Straight
        elif this.IsThreeOfAKind() then ThreeOfAKind
        elif this.IsTwoPair() then TwoPair
        elif this.IsOnePair() then OnePair
        else HighCard
    
    override this.ToString() = sprintf "%s - %s" (String.Join('|', cards)) (string this.Rank)
    
    member this.IsStraightFlush() = this.IsStraight() && this.IsFlush()
    member this.IsFourOfAKind() = 4 = snd cardRankCounts[0]
    member this.IsFullHouse() = (3 = snd cardRankCounts[0]) && (2 = snd cardRankCounts[1])
    member this.IsFlush() = this.Cards |> Array.forall (fun c -> c.Suit = this.Cards[0].Suit)
    member this.IsStraight() =
        let isNormalStraight = (4 = ((int this.Cards[0].Rank) - (int this.Cards[4].Rank))) &&
                                cardRankCounts.Length = 5
        let isAceLowStraight = this.Cards[0].Rank = CardRank.Ace &&
                               this.Cards[4].Rank = CardRank.Two &&
                               this.Cards[3].Rank = CardRank.Three &&
                               this.Cards[2].Rank = CardRank.Four &&
                               this.Cards[1].Rank = CardRank.Five
        isNormalStraight || isAceLowStraight
    member this.IsThreeOfAKind() = 3 = snd cardRankCounts[0]
    member this.IsTwoPair() = (2 = snd cardRankCounts[0]) && (2 = snd cardRankCounts[1])
    member this.IsOnePair() = (2 = snd cardRankCounts[0]) && (2 <> snd cardRankCounts[1])
    
    interface IComparable with
        member this.CompareTo(otherObj) =
            match otherObj with
            | :? Hand as other ->
                let handRankCmp = compare this.Rank other.Rank
                if handRankCmp <> 0 then handRankCmp
                else
                    match this.Rank with
                    | FourOfAKind | FullHouse | ThreeOfAKind ->
                        compare (fst this.CardRankCounts[0]) (fst other.CardRankCounts[0])
                    | TwoPair ->
                        let highPairCmp = compare (fst this.CardRankCounts[0]) (fst other.CardRankCounts[0])
                        let lowPairCmp = compare (fst this.CardRankCounts[1]) (fst other.CardRankCounts[1])
                        if highPairCmp <> 0 then highPairCmp
                        elif lowPairCmp <> 0 then lowPairCmp
                        else compare (fst this.CardRankCounts[2]) (fst other.CardRankCounts[2])
                    | OnePair ->
                        let pairCmp = compare (fst this.CardRankCounts[0]) (fst other.CardRankCounts[0])
                        if pairCmp <> 0 then pairCmp
                        else compare this.Cards other.Cards
                    | _ ->  compare this.Cards other.Cards
            | _ -> raise <| ArgumentException("Can't compare instances of different types")

//[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    let p1 = new Hand("8D 8S 6S 6C 4C")
    let p2 = new Hand("QH TC 9D 7D 6D")
    printfn $"Player 1 wins: {p1 > p2}"

    let lines = File.ReadLines @"..\..\..\000\50\Euler54.txt" |> Seq.toList
    let cards = lines
                |> Seq.map (fun s -> s.Split(' ') |> Array.map cardFromString)
                |> Seq.toList
    let hands = cards
                |> Seq.map (fun arr -> Array.chunkBySize 5 arr)
                |> Seq.map (fun arr -> (new Hand(arr[0]), new Hand(arr[1])))
                |> Seq.toList
    let results = hands |> List.map (fun h -> (h, fst h > snd h))
    //printfn "%s" (String.Join("\n", results))
    let wins = hands |> List.filter (fun players -> fst players > snd players)
    printfn $"Player 1 wins {wins.Length} hands."
    
    //let royalFlush = new Hand([|{Suit = Diamonds; Rank = CardRank.Ace};
    //                            {Suit = Diamonds; Rank = CardRank.Queen};
    //                            {Suit = Diamonds; Rank = CardRank.King};
    //                            {Suit = Diamonds; Rank = CardRank.Ten};
    //                            {Suit = Diamonds; Rank = CardRank.Jack}|])
    //printfn $"{royalFlush}"
    
    //let straightFlush = new Hand([|{Suit = Hearts; Rank = CardRank.Five};
    //                            {Suit = Hearts; Rank = CardRank.Three};
    //                            {Suit = Hearts; Rank = CardRank.Four};
    //                            {Suit = Hearts; Rank = CardRank.Two};
    //                            {Suit = Hearts; Rank = CardRank.Six}|])
    //printfn $"{straightFlush}"
 
    //let fourOfAKind = new Hand([|{Suit = Hearts; Rank = CardRank.Five};
    //                    {Suit = Clubs; Rank = CardRank.Five};
    //                    {Suit = Spades; Rank = CardRank.Five};
    //                    {Suit = Hearts; Rank = CardRank.Three};
    //                    {Suit = Diamonds; Rank = CardRank.Five}|])
    //printfn $"{fourOfAKind}"
 
    //let fullHouse = new Hand([|{Suit = Hearts; Rank = CardRank.Five};
    //                    {Suit = Clubs; Rank = CardRank.Five};
    //                    {Suit = Spades; Rank = CardRank.Five};
    //                    {Suit = Hearts; Rank = CardRank.Three};
    //                    {Suit = Diamonds; Rank = CardRank.Three}|])
    //printfn $"{fullHouse}"
 
    //let flush = new Hand([|{Suit = Clubs; Rank = CardRank.Five};
    //                    {Suit = Clubs; Rank = CardRank.Ten};
    //                    {Suit = Clubs; Rank = CardRank.Four};
    //                    {Suit = Clubs; Rank = CardRank.Nine};
    //                    {Suit = Clubs; Rank = CardRank.Six}|])
    //printfn $"{flush}"
 
    //let aceLowStraight = new Hand([|{Suit = Hearts; Rank = CardRank.Five};
    //                    {Suit = Clubs; Rank = CardRank.Three};
    //                    {Suit = Hearts; Rank = CardRank.Four};
    //                    {Suit = Hearts; Rank = CardRank.Two};
    //                    {Suit = Hearts; Rank = CardRank.Ace}|])
    //printfn $"{aceLowStraight}"
 
    //let sixHighStraight = new Hand([|{Suit = Hearts; Rank = CardRank.Five};
    //                    {Suit = Clubs; Rank = CardRank.Three};
    //                    {Suit = Hearts; Rank = CardRank.Four};
    //                    {Suit = Hearts; Rank = CardRank.Two};
    //                    {Suit = Hearts; Rank = CardRank.Six}|])
    //printfn $"{sixHighStraight}"
 
    //let threeOfAKind = new Hand([|{Suit = Hearts; Rank = CardRank.Five};
    //                    {Suit = Clubs; Rank = CardRank.Five};
    //                    {Suit = Spades; Rank = CardRank.Five};
    //                    {Suit = Hearts; Rank = CardRank.Four};
    //                    {Suit = Diamonds; Rank = CardRank.Three}|])
    //printfn $"{threeOfAKind}"
 
    //let twoPair = new Hand([|{Suit = Hearts; Rank = CardRank.Five};
    //                    {Suit = Clubs; Rank = CardRank.Five};
    //                    {Suit = Spades; Rank = CardRank.Four};
    //                    {Suit = Hearts; Rank = CardRank.Four};
    //                    {Suit = Diamonds; Rank = CardRank.Three}|])
    //printfn $"{twoPair}"
 
    //let onePair = new Hand([|{Suit = Hearts; Rank = CardRank.Five};
    //                    {Suit = Clubs; Rank = CardRank.Five};
    //                    {Suit = Spades; Rank = CardRank.Ten};
    //                    {Suit = Hearts; Rank = CardRank.Four};
    //                    {Suit = Diamonds; Rank = CardRank.Three}|])
    //printfn $"{onePair}"
 
    //let highCard = new Hand([|{Suit = Hearts; Rank = CardRank.King};
    //                    {Suit = Clubs; Rank = CardRank.Five};
    //                    {Suit = Spades; Rank = CardRank.Ten};
    //                    {Suit = Hearts; Rank = CardRank.Four};
    //                    {Suit = Diamonds; Rank = CardRank.Three}|])
    //printfn $"{highCard}"

    //printfn $"{royalFlush} is higher than {straightFlush}: {(compare royalFlush straightFlush) > 0}"
    //printfn $"{straightFlush} is higher than {fourOfAKind}: {(compare straightFlush fourOfAKind) > 0}"
    
    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0