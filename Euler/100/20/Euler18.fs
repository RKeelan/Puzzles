module Euler18

(*
By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

3
7 4
2 4 6
8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom of the triangle below:

75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route.
However, Problem 67, is the same challenge with a triangle containing one-hundred rows; it cannot
be solved by brute force, and requires a clever method! ;o)

*)

(*
The correct way to solve this is the A* algorithm
But what data structure should I use for the pyramid?
    * I could do a tree, but how would I make it?
    * Another option is a jagged array, but I think traversing that would be a pain

Each ith node in row N leads to node i and i+1 in row N+1
*)

open System
open System.Linq
open System.Collections.Generic
open FSharpx.Collections

// Path-finding -----------------------------------------------------------------------------------

type Node = {
    Row : int
    Col : int
 } with 
    override this.ToString() = $"({this.Row}, {this.Col})"

type Path(nodePath : Node list, w : int) =
    let nodes = nodePath
    let weight = w
    member this.Nodes = nodes
    member this.Node = nodes.Head
    member this.Weight = weight
    override this.Equals(otherObj:obj) = 
        match otherObj with
        | :? Path as other -> (this.Nodes = other.Nodes) && (this.Weight = other.Weight)
        | _ -> false
    override this.GetHashCode() =
        let mutable hash = 17
        hash <- hash * 23 + this.Nodes.GetHashCode()
        hash <- hash * 23 + this.Weight.GetHashCode()
        hash
    override this.ToString() = String.Join(" -> ", this.Nodes)
    interface IComparable with
        member this.CompareTo(otherObj) =
            match otherObj with
            | :? Path as other -> compare this.Weight other.Weight
            | _ -> raise <| ArgumentException("Can't compare instances of different types")

let getParent (maxWeight:int) (array:int[][]) (path : Path) (isLeftParent : bool) : option<Path> =
    let nextRowIndex = path.Node.Row - 1
    let nextColIndex =  if isLeftParent then (path.Node.Col - 1) else path.Node.Col
    if (nextColIndex < 0) || nextColIndex >= array.[nextRowIndex].Length then None
    else
        let newNode = {Row = nextRowIndex; Col = nextColIndex}
        Some(new Path((newNode :: path.Nodes), (path.Weight + (maxWeight - array.[nextRowIndex].[nextColIndex]))))

let rec search (maxWeight:int) (array : int[][]) (priorityQueue : IPriorityQueue<Path>) : Path =
    let (candidatePath, queue) = PriorityQueue.pop priorityQueue // Get the "best" path
    printfn $"\tExamining {candidatePath}"
    if candidatePath.Node.Row = 0 then candidatePath
    else
        // RK 18-Dec-2021: Given the shape of the, the only node eith neither a left parent nor a
        // right parent is the top one, and the algorithm stops before it tries finding that node's
        // parents
        let leftPath = getParent maxWeight array candidatePath true
        let rightPath = getParent maxWeight array candidatePath false
        if leftPath.IsNone then search maxWeight array (PriorityQueue.insert rightPath.Value queue)
        else
            let intermediateQueue = PriorityQueue.insert leftPath.Value queue
            if rightPath.IsNone then search maxWeight array intermediateQueue
            else search maxWeight array (PriorityQueue.insert rightPath.Value intermediateQueue)

let findPath (maxWeight:int) (array:int[][]) : Path =
    let mutable priorityQueue = PriorityQueue.empty false
    let bottomRow = (array.Length - 1)
    for i = 0 to array.[bottomRow].Length - 1 do
        let path = new Path([{Row = bottomRow; Col = i}], (maxWeight - array.[bottomRow].[i]))
        // RK 18-Dec-2021: This is quite contrary to the spirit of functional programming, but I'm
        // also quite tired at the moment
        priorityQueue <- PriorityQueue.insert path priorityQueue
        //printfn $"Value at ({bottomRow}, {i})  is {array.[bottomRow].[i]}"
    search maxWeight array priorityQueue

let pathSum (array:int[][]) (path:Path) : int =
    path.Nodes |> List.fold (fun acc node -> (acc + array.[node.Row].[node.Col])) 0

//[<EntryPoint>]
let main argv =
    let testArray = [|[|3|]
                      [|7;4|]
                      [|2;4;6|]
                      [|8;5;9;3|]|]

    printfn "Searching for path in test array:"
    Util.print2DArray testArray
    let testPath = findPath 10 testArray
    printfn $"Found path [{testPath}] with sum {pathSum testArray testPath}"

    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let mainArray = [|[|75|]
                      [|95;64|]
                      [|17;47;82|]
                      [|18;35;87;10|]
                      [|20;04;82;47;65|]
                      [|19;01;23;75;03;34|]
                      [|88;02;77;73;07;63;67|]
                      [|99;65;04;28;06;16;70;92|]
                      [|41;41;26;56;83;40;80;70;33|]
                      [|41;48;72;33;47;32;37;16;94;29|]
                      [|53;71;44;65;25;43;91;52;97;51;14|]
                      [|70;11;33;28;77;73;17;78;39;68;17;57|]
                      [|91;71;52;38;17;14;91;43;58;50;27;29;48|]
                      [|63;66;04;68;89;53;67;30;73;16;69;87;40;31|]
                      [|04;62;98;27;23;09;70;98;73;93;38;53;60;04;23|]|]
    printfn "Searching for path in main array:"
    Util.print2DArray mainArray
    let path = findPath 100 mainArray
    printfn $"Found path [{path}] with sum {pathSum mainArray path}"

    stopWatch.Stop()
    printfn $"Execution time: {stopWatch.Elapsed.TotalMilliseconds:N0} ms." 
    0