// Learn more about F# at http://fsharp.org

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

let readPiece step stackNo =
    printf "Enter a piece character (space for blank) for the %A piece on stack %i: " (StepToPlace step) stackNo

    let c = Console.ReadKey().KeyChar
    Console.Clear()

    match c with
    | ' ' -> None
    | x -> Some(Piece x)

let readStack stackNo =
    { stack =
          [ 0 .. 3 ]
          |> List.choose (fun i -> readPiece i stackNo)
      index = Index stackNo }

let rec readInitialState stacks =
    [ 1 .. stacks ] |> List.map readStack |> State

let GenerateMoves (s: Node<State>) (queue : Node<State> list) (visited : Node<State> list): Node<State> list =
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
        | (s :: _, []) -> true
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

    let getState n = n.value
    let getStack (State st) = st
    let getSortedQV = (List.map (fun m -> List.sortBy string  (getStack (getState m))) (queue)) @ (List.map (fun mv -> getStack(getState mv)) visited)
    
    let getSortedNode move = let (State ns) = move.value
                             List.sortBy string ns
    
    s.value
    |> generatePairs
    |> List.filter allowedMove
    |> List.map makeMove
    |> List.map (makeState s.value)
    |> List.map (fun state -> { value = state; parent = Some s; depth = (s.depth + 1) }) //This is all our possible new moves
    |> List.distinctBy (fun n -> ( let (State ns) = n.value
                                   List.sortBy string ns))
    |> List.filter (fun move -> ( let nsl = getSortedNode move
                                  not(List.contains nsl (getSortedQV)) ) )    

let CheckSolved (State s) =
    let CheckStack st =
        (List.isEmpty st.stack)
        || ((List.forall (fun x -> (x = st.stack.Head)) st.stack)
            && (List.length st.stack) = maxStackSize)

    List.forall CheckStack s

let Solve (State s) =
    let sortNode move = let (State ns) = move.value
                        let so = List.sortBy string ns
                        {move with value = (State so)}
    
    let getSortedNode move = let (State ns) = move.value
                             List.sortBy string ns
    
    let pSolve (pred) (visited) (queue) =
        async {
            return List.fold (fun acc m -> match acc with
                                           | (_,v,Some y) -> ([],v,Some y)
                                           | (_,v,None) when pred m.value -> ([],(sortNode m)::v,Some m)
                                           | (q,v,None) -> (q @ (GenerateMoves m queue v),(sortNode m)::v,None)
                                           ) ([],visited,None) queue
        }
        
    let rec pSolve' (pred) (queue) (visited) (split) =        
        let taskResult = queue
                          |> List.splitInto split
                          |> List.map (pSolve pred visited)
                          |> Async.Parallel
                          |> Async.RunSynchronously
                          |> Array.toList
                          |> List.fold(fun acc r -> let (q,v,x) = acc
                                                    match r with
                                                    | (_,_,Some x2) -> ([],[],Some x2)
                                                    | (q2,v2,None) -> (List.distinctBy getSortedNode (q2 @ q), List.distinctBy getSortedNode (v2 @ v), None)) ([],[],None)
        match taskResult with
        | (_,_,Some x) -> Some x
        | ([],_,None) -> None
        | ((x::xs),v,None) -> pSolve' pred ((log ("Reached Depth:" + x.depth.ToString() + " with Queue Size:" + (xs.Length + 1).ToString()) x)::xs) v split
 
    
    pSolve' CheckSolved [ { value = State s; parent = None; depth = 0 } ] [] numThreads

[<EntryPoint>]
let main argv =
    try
        Console.Write("Enter the number of stacks: ")
        let numberOfStacks = Console.ReadLine() |> int
        let initState = readInitialState numberOfStacks
        Console.WriteLine(initState)
        match (Solve initState) with
        | None -> printfn "No solution found"
        | Some x -> printfn "%s" (x.ToString())
    with :? FormatException -> printfn "Invalid Number"

    0 // return an integer exit code
