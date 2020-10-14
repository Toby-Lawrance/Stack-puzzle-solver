﻿// Learn more about F# at http://fsharp.org

open System
open System.Collections.Immutable

let log (value) (otherThing) =
    printfn "%s" value
    otherThing
    
let logValue (prefix) (value) =
    printfn "%s%s" prefix (value.ToString())
    value

[<Literal>]
let maxStackSize = 4

[<Literal>]
let numThreads = 12

type Piece = Piece of char

type Index = Index of int

[<StructuredFormatDisplay("{AsString}");CustomEquality;NoComparison>]
type Stack =
    { stack: Piece list
      index: Index }
    override this.ToString() =
        this.stack
        |> Array.ofList
        |> Array.map (fun (Piece c) -> c)
        |> String.Concat
    member this.AsString =
        this.ToString()
    override this.GetHashCode() =
        this.stack.GetHashCode()
        
    override this.Equals(arg) =
        match arg with
        | :? Stack as s -> this.stack = s.stack
        | _ -> false

type State =
    | State of Stack list
    override this.ToString() =
        let (State s) = this

        s
        |> List.map (sprintf "(%A)")
        |> String.concat ","
        |> sprintf "[%s]"

type Place =
    | Top
    | Second
    | Third
    | Bottom

type Node<'a> =
    { value: 'a
      parent: Node<'a> option
      depth: int }
    override this.ToString() =
        Some this
        |> List.unfold (Option.map (fun c -> (c.value,c.parent)))
        |> List.rev
        |> List.map string
        |> String.concat "\n"

let StepToPlace (step: int) =
    match step with
    | 0 -> Top
    | 1 -> Second
    | 2 -> Third
    | 3 -> Bottom
    | _ -> failwith "Value off the stack"

let makePiece c =
    match c with
    | ' ' -> None
    | x -> Some(Piece x)

let readPiece step stackNo =
    printf "Enter a piece character (space for blank) for the %A piece on stack %i: " (StepToPlace step) stackNo

    let c = Console.ReadKey().KeyChar
    Console.Clear()
    makePiece c

let readStack stackNo =
    { stack =
          [ 0 .. 3 ]
          |> List.choose (fun i -> readPiece i stackNo)
      index = Index stackNo }

let rec readInitialState stacks =
    [ 1 .. stacks ] |> List.map readStack |> State

let getSortedStackList move = let (State gSl) = move.value
                              List.sortBy string gSl
                              
let GenerateMoves (s) (queue) (visited) =
    let generatePairs (State state) =
        query {
            for stack1 in state do
                join stack2 in state on (true = true)
                where (stack1.index <> stack2.index)
                select (stack1, stack2)
        }
        |> Seq.toList

    let allowedMove (source, target) =
        match (source.stack, target.stack) with
        | (s :: _, t :: _) when (s = t)
                                && (List.length target.stack) < maxStackSize -> true
        | (_ :: _, []) -> true
        | (_, _) -> false

    let rec makeMove (source, target) =
        match (source.stack, target.stack) with
        | ((s :: ss), ts) ->
            match ({ source with stack = ss }, { target with stack = (s :: ts) }) with
            | (so, ta) when allowedMove (so, ta) -> makeMove (so, ta)
            | (so, ta) -> (so, ta)
        | _ -> failwith "We were wrong about this never happening in makeMove"

    let makeState (State oldState) (source, target) =
        oldState
        |> List.map (function
            | s when s.index = source.index -> source
            | s when s.index = target.index -> target
            | s -> s)
        |> State

    let getSortedQV = visited @ (List.map(fun move -> getSortedStackList move) queue)
    
    s.value
    |> generatePairs
    |> List.filter allowedMove
    |> List.map makeMove
    |> List.map (makeState s.value)
    |> List.map (fun state -> { value = state; parent = Some s; depth = (s.depth + 1) }) //This is all our possible new moves
    |> List.distinctBy getSortedStackList
    |> List.filter (fun move -> not (List.contains (getSortedStackList move) getSortedQV))

let CheckSolved (s) =
    let CheckStack (st) =
        (List.isEmpty st.stack)
        || ((List.forall (fun x -> (x = st.stack.Head)) st.stack)
            && (List.length st.stack) = maxStackSize)

    let (State stat) = s.value
    List.forall CheckStack (stat)

let Solve (State s) =    
    let pSolve (visited) (queue) =
        async {
            return List.fold (fun (q,v) m -> (q @ (GenerateMoves m queue v), (getSortedStackList m) :: v)) ([],visited) queue
        }
        
    let rec pSolve' (queue) (visited) =        
        let (newQueue,newVisited) = queue
                                  |> List.splitInto numThreads
                                  |> List.map (pSolve visited)
                                  |> Async.Parallel
                                  |> Async.RunSynchronously
                                  |> Array.fold(fun (acc_q,acc_v) (q,v) -> (List.distinctBy getSortedStackList (acc_q @ q),List.distinct (acc_v @ v))) ([],[])                                  
        
        if List.isEmpty newQueue then
            None
        else
            Console.WriteLine("Depth: " + newQueue.Head.depth.ToString() + " Queue size: " + newQueue.Length.ToString() + " Visited Size: " + newVisited.Length.ToString())
            List.tryFind CheckSolved newQueue
            |> Option.orElseWith (fun () -> pSolve' newQueue newVisited)
        
 
    
    pSolve' [ { value = State s; parent = None; depth = 0 } ] []

[<EntryPoint>]
let main argv =
    try
        Console.Write("Enter the number of stacks: ")
        let numberOfStacks = Console.ReadLine() |> int
        let initState = readInitialState numberOfStacks
        match (Solve initState) with
        | None -> printfn "No solution found"
        | Some x -> printfn "%s" (x.ToString())
    with :? FormatException -> printfn "Invalid Number"

    0 // return an integer exit code
