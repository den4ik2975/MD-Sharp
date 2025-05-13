module Eval
open AST
open System

// Обработка ошибок
exception RuntimeTypeError of string
let typeErr fmt = Printf.ksprintf (fun s -> raise (RuntimeTypeError s)) fmt

let funof = function
    | "+" -> (function
              | [Int a; Int b] -> Int (a + b)
              | xs             -> typeErr "'+' expects two ints, got %A" xs)

    | "-" -> (function
              | [Int a; Int b] -> Int (b - a)
              | xs             -> typeErr "'-' expects two ints, got %A" xs)

    | "*" -> (function
              | [Int a; Int b] -> Int (a * b)
              | xs             -> typeErr "'*' expects two ints, got %A" xs)

    | "/" -> (function
              | [Int 0; _]    -> typeErr "division by zero"
              | [Int a; Int b] -> Int (b / a)
              | xs             -> typeErr "'/' expects two ints, got %A" xs)

    | "="  -> (function
               | [Int a; Int b] -> Bool (a = b)
               | xs             -> typeErr "'=' expects two ints, got %A" xs)

    | "!=" -> (function
               | [Int a; Int b] -> Bool (a <> b)
               | xs             -> typeErr "'!=' expects two ints, got %A" xs)

    | ">"  -> (function
               | [Int a; Int b] -> Bool (b > a)
               | xs             -> typeErr "'>' expects two ints, got %A" xs)

    | "<"  -> (function
               | [Int a; Int b] -> Bool (b < a)
               | xs             -> typeErr "'<' expects two ints, got %A" xs)

    | "<=" -> (function
               | [Int a; Int b] -> Bool (b <= a)
               | xs             -> typeErr "'<=' expects two ints, got %A" xs)

    | ">=" -> (function
               | [Int a; Int b] -> Bool (b >= a)
               | xs             -> typeErr "'>=' expects two ints, got %A" xs)

    | "head" -> (function
                 | [Cons(h,_)] -> h
                 | [Nil]       -> typeErr "head: empty list"
                 | xs          -> typeErr "head expects one non-empty list, got %A" xs)

    | "tail" -> (function
                 | [Cons(_,t)] -> t
                 | [Nil]       -> typeErr "tail: empty list"
                 | xs          -> typeErr "tail expects one non-empty list, got %A" xs)
    
    | "read"  -> (function
         | [] ->
             let s = Console.ReadLine()
             Str s
         | xs -> typeErr "'read' expects no arguments, got %A" xs)

    | "print" -> (function
        | [v] ->
         let toText = function
             | Int n  -> string n
             | Bool b -> string b
             | Str s  -> s
             | Nil    -> "[]"
             | other  -> sprintf "%A" other
         Console.WriteLine(toText v)
         v                                                          // возвращаем то же значение
        | xs -> typeErr "'print' expects one argument, got %A" xs)

    | id -> fun _ -> typeErr "unknown primitive %s" id


let rec eval exp env =    
    match exp with
        | Int(n) -> Int(n)
        | Str s -> Str s
        | Bool b -> Bool b
        | Var(x) -> Map.find x env
        | Lam(id,ex) -> Closure(exp,env)
        | App(ex1,ex2) -> apply (eval ex1 env) (eval ex2 env)
        
        | PFunc id ->                           
            let arity =
                match id with
                | "head" | "tail" -> 1
                | "print"         -> 1
                | "read"          -> 0
                | _               -> 2
            if arity = 0 then                  
                (funof id) []                   
            else
                Op(id, arity, [])

        | Comment _ -> Var "()"

        | Nil -> Nil                            // []
        | Cons(h,t) ->                          // head:-:tail
            Cons(eval h env, eval t env)

        | Range(a,b) ->                         // [a..b]
            let rec build x y =
                if x > y then Nil
                else Cons(Int x, build (x+1) y)
            match eval a env, eval b env with
            | Int x, Int y -> build x y
            | _ -> failwith "range bounds must be integers"
            
        | Cond(p,e1,e2) ->
            match eval p env with
                | Bool false | Int 0 -> eval e2 env
                | Bool true | Int _  -> eval e1 env
                | x                  -> typeErr "if-condition expects Bool or Int, got %A" x

                
        | Let(id,e1,e2) ->
            let e1' = eval e1 env in eval e2 (Map.add id e1' env)
        | LetRec(id,e1,e2) ->
            let e1' = RClosure(e1,env,id) in eval e2 (Map.add id e1' env)
            
        | Op _ | Closure _ | RClosure _ -> exp
            
and apply e1 e2 =
    match e1 with
        | Closure(Lam(v,e),env) -> eval e (Map.add v e2 env)
        | RClosure(Lam(v,e),env, id) -> eval e (Map.add v e2 (Map.add id e1 env))
        | Op(id,n,args) ->
            if n=1 then (funof id)(e2::args)
            else Op(id,n-1,e2::args)
            
        | v -> typeErr "application of non-function value %A" v

module Runner =
    /// Выполняем последовательность выражений,
    /// возвращаем значение последнего
    let evalProgram (prog:expr) =
        try
            let mutable env : env = Map.empty
            let res = eval prog env 
            Ok res
        with RuntimeTypeError msg -> Error msg

// наружу
[<CompiledName("evalProgram")>]
let evalProgram = Runner.evalProgram

