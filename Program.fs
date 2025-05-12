open System
open System.IO
open Parser
open Eval
open AST

let rec printExpr indent expr =
    let indentStr = String.replicate indent " "
    match expr with
    | Int n -> printfn "%s- Int: %d" indentStr n
    | Bool b -> printfn "%s- Bool: %b" indentStr b
    | Var id -> printfn "%s- Var: %s" indentStr id
    | App(e1, e2) ->
        printfn "%s- App:" indentStr
        printExpr (indent + 2) e1
        printExpr (indent + 2) e2
    | Lam(id, e) ->
        printfn "%s- Lambda (param: %s):" indentStr id
        printExpr (indent + 2) e
    | PFunc id -> printfn "%s- PFunc: %s" indentStr id
    | Cond(e1, e2, e3) ->
        printfn "%s- Conditional:" indentStr
        printfn "%s  Condition:" indentStr
        printExpr (indent + 4) e1
        printfn "%s  Then:" indentStr
        printExpr (indent + 4) e2
        printfn "%s  Else:" indentStr
        printExpr (indent + 4) e3
    | Let(id, e1, e2) ->
        printfn "%s- Let (var: %s):" indentStr id
        printfn "%s  Value:" indentStr
        printExpr (indent + 4) e1
        printfn "%s  In:" indentStr
        printExpr (indent + 4) e2
    | LetRec(id, e1, e2) ->
        printfn "%s- LetRec (var: %s):" indentStr id
        printfn "%s  Value:" indentStr
        printExpr (indent + 4) e1
        printfn "%s  In:" indentStr
        printExpr (indent + 4) e2
    | Op(id, n, args) ->
        printfn "%s- Op: %s (arity: %d)" indentStr id n
        args |> List.iteri (fun i arg ->
            printfn "%s  Arg %d:" indentStr i
            printExpr (indent + 4) arg)
    | Closure(e, env) ->
        printfn "%s- Closure:" indentStr
        printExpr (indent + 2) e
        printfn "%s  Environment: %A" indentStr env
    | RClosure(e, env, id) ->
        printfn "%s- RClosure (recursive var: %s):" indentStr id
        printExpr (indent + 2) e
        printfn "%s  Environment: %A" indentStr env
    | Range(e1, e2) ->
        printfn "%s- Range:" indentStr
        printfn "%s  Start:" indentStr
        printExpr (indent + 4) e1
        printfn "%s  End:" indentStr
        printExpr (indent + 4) e2
    | ReadFile path ->
        printfn "%s- ReadFile:" indentStr
        printfn "%s  Path:" indentStr
        printExpr (indent + 4) path
    | WriteFile(path, content) ->
        printfn "%s- WriteFile:" indentStr
        printfn "%s  Path:" indentStr
        printExpr (indent + 4) path
        printfn "%s  Content:" indentStr
        printExpr (indent + 4) content
    | Comment string ->
        printfn "%s- Comment: %s" indentStr string
    | Str string ->
        printfn "%s- String: %s" indentStr string
        
[<EntryPoint>]
let main argv =
    let source =
        if argv.Length = 0 then
            stdin.ReadToEnd()            // вместо CLI.readStdin ()
        else
            File.ReadAllText argv[0]

    // Replace the current match statement
    let parseResult =
        try
            Ok (Parser.parseProgram source)
        with e -> Error e.Message

    match parseResult with
    | Error msg -> eprintfn "Parse error:\n%s" msg ; 1
    | Ok   ast  ->
        ast |> List.iteri (fun i node -> 
                    printfn "\nTop-level node %d:" i
                    printExpr 2 node)
        match Eval.evalProgram ast with
        | Ok v    -> printfn "%A" v ; 0
        | Error e -> eprintfn "Runtime error: %s" e ; 1