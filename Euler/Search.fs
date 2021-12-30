module Search

open System
open FSharpx.Collections

[<CustomEquality>]
[<CustomComparison>]
type AStarNode<'a> when 'a : equality = {
    Cost : int
    Item : 'a
} with
    // RK 29-Dec-2021: Note that I'm excluding the cost here. I'm not sure if that will interfere
    // with accounting for reaching the same node with two different costs, below
    override this.Equals(otherObj:obj) = 
        match otherObj with
        | :? AStarNode<'a> as other -> (this.Item = other.Item)
        | _ -> false
    override this.GetHashCode() =
        let mutable hash = 17
        hash <- hash * 23 + this.Item.GetHashCode()
        hash
    override this.ToString() = $"{this.Item} with {this.Cost} cost"
    interface IComparable with
        member this.CompareTo(otherObj) =
            match otherObj with
            | null -> 1
            | :? AStarNode<'a> as other ->
                // RK 29-Dec-2021:
                // On the one hand, I want to use only Cost to priorities the nodes.
                // On the other hand, ComapreTo is used by Map.ContainsKey, and for that,
                // I want two nodes with equal items to register as equal
                //
                // I think the problem with this logic is that Map is implemented with some kind
                // of tree, so if costs differ, two nodes might not be compared to each other,
                // which means this won't work
                //
                // So this implemention isn't really workable. On the other hand, I don't really
                // need full A* for my current problem (Euler44), so I'll just copy this code there
                // and hack it until it works
                if this = other then 0
                else compare this.Cost other.Cost
            | _ -> raise <| ArgumentException("Can't compare instances of different types")


(*
Note that there are some deficiencies in this implementation relative to proper A*
    - If there's two ways to get to a particular node, and the more expensive path is found first,
    I'll never update to the cheaper one
    - I return the goal node, but not the path to get there
    - I won't implement that stuff unles I need it
*)
let aStarSearch (start:'a)
                (isGoal:'a -> bool)
                (getNeighbours:'a->seq<(int*'a)>)
                : 'a =
    // The nodes that still need to be visited
    let openSet = PriorityQueue.empty false |> PriorityQueue.insert {Cost = 0; Item = start}

    // For node n, predecesors[n] is the predecesor of node n on the cheapest path
    let mutable predecesors = Map.empty

    let rec search (queue:IPriorityQueue<AStarNode<'a>>) : 'a =
        let mutable (currentNode, newQueue) = queue.Pop()
        //printfn $"Visiting {currentNode}"
        if isGoal currentNode.Item then currentNode.Item
        else
            for (cost, item) in (getNeighbours currentNode.Item) do
                let node = {Cost = cost; Item = item}
                if not(predecesors.ContainsKey(node)) then
                    predecesors <- predecesors.Add(node, currentNode)
                    newQueue <- newQueue.Insert(node)

            search newQueue

    search openSet

