open System.IO
open Parser
//open ParserDebug // можете включить дебаг-режим, подключив модуль ParserDebug
open Eval


[<EntryPoint>]
let main argv =
    let source =
        if argv.Length = 0 then
            stdin.ReadToEnd()
        else
            File.ReadAllText argv[0]

    let parseResult =
        try
            Ok (parseProgram source)
        with e -> Error e.Message

    match parseResult with
    | Error msg -> eprintfn "Parse error:\n%s" msg ; 1
    | Ok   ast  ->
        match evalProgram ast with
        | Ok v    -> printfn "%A" v ; 0
        | Error e -> eprintfn "Runtime error: %s" e ; 1