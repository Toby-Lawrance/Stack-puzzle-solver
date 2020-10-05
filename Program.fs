// Learn more about F# at http://fsharp.org

open System
open System.Collections.Immutable

type Piece = char

type Index = Index of int
type Stack = { stack: Piece list; index: Index }
    with override this.ToString () =
            this.stack
            |> Array.ofList
            |> String.Concat
            |> sprintf "(%s)"

type State = Stack list

type Place =
    | Top
    | Second
    | Third
    | Bottom

type Node<'a> = { value: 'a; parent: Node<'a> option }

let StepToPlace (step: int) =
    match step with
    | 0 -> Top
    | 1 -> Second
    | 2 -> Third
    | 3 -> Bottom
    | _ -> failwith "Value off the stack"

let StackToString s =
    s
    |> List.mapFold (fun acc elem ->
        let str = elem.ToString()
        str, sprintf "%s,%s" acc str) ""
    |> snd
    |> sprintf "(%s)"


let StateToString s =
    // TODO delete me
    s
    |> List.mapFold (fun acc stack ->
        let str = StackToString stack
        str, sprintf "%s,%s" acc str) ""
    |> snd
    |> sprintf "[%s]"


let ReadPiece (step) (stackNo): Piece =
    printf "Enter a piece character (space for blank) for the %A piece on stack %i:" (StepToPlace step) stackNo

    let c = Console.ReadKey().KeyChar
    Console.Clear()
    c



let ReadStack (stackNo: int): ImmutableStack =
    //(ReadPiece 0 stackNo, ReadPiece 1 stackNo, ReadPiece 2 stackNo, ReadPiece 3 stackNo)
    // TODO implement me
    failwith "TODO"

let rec ReadInitialState (stacks: int) (builtState: State): State =
    match stacks with
    | 0 -> builtState
    | _ -> ReadInitialState (stacks - 1) (ReadStack stacks :: builtState)

let GenerateMoves (s) =
    let GenerateStateFromMove (baseNode) (f, t) (f2, t2) = None
    let GenerateMove (baseNode) (f, t): Node<State> option = None

    let rec GenerateMovesFromStack (baseNode: Node<State> option)
                                   (s: ImmutableStack)
                                   (o: ImmutableStack list)
                                   (stacksMoves: Node<State> list)
                                   : Node<State> list =
        match o with
        | [] -> stacksMoves
        | x :: xs ->
            let move = GenerateMove baseNode (s, x)

            match move with
            | Some mv -> GenerateMovesFromStack baseNode s xs (mv :: stacksMoves)
            | _ -> GenerateMovesFromStack baseNode s xs stacksMoves

    let rec GenerateMoves' (baseNode: Node<State> option)
                           (postState: State)
                           (preState: State)
                           (foundMoves: Node<State> list)
                           : Node<State> list =
        match postState with
        | [] -> foundMoves
        | x :: xs ->
            let moves =
                GenerateMovesFromStack baseNode x (xs @ preState)

            GenerateMoves' baseNode xs (x :: preState) (moves foundMoves)

    GenerateMoves' s s.Value.value [] []

let CheckSolved (s: State) =
    let CheckStack st = true
    List.forall CheckStack s


let Solve (s: State) =
    let rec Solve' (pred) (queue) =
        match queue with
        | [] -> None
        | x :: _ when pred x.value -> Some x
        | x :: xs -> Solve' pred (xs @ GenerateMoves(Some x))

    Solve' CheckSolved [ { value = s; parent = None } ]

let generatePairs (state: Stack list) =
    query {
        for stack1 in state do
            join stack2 in state on (true = true)
            where (stack1.index <> stack2.index)
            select (stack1, stack2)
    }


[<EntryPoint>]
let main argv =
    try
        Console.Write("Enter the number of stacks: ")
        let x = Console.ReadLine()
        let initState = ReadInitialState (int (x)) []
        Console.WriteLine(StateToString initState)
        Console.WriteLine((Solve initState))
    with :? FormatException -> printfn "Invalid Number"

    0 // return an integer exit code
