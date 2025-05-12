open System
open System.IO
open Parser // можете включить дебаг-режим, подключив модуль ParserDebug
open Eval
open AST

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
        match evalProgram ast with
        | Ok v    -> printfn "%A" v ; 0
        | Error e -> eprintfn "Runtime error: %s" e ; 1