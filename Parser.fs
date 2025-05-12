module Parser
open AST
open FParsec

let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply
        
// Parser module
// Helper parsers
let ws = spaces
let str_ws s = pstring s >>. ws
let skipNewline = skipChar '\n' <|> (skipChar '\r' .>> skipChar '\n')

// Forward references for recursive parsers
let expr, exprRef = createParserForwardedToRef<expr, unit>()

// Basic value parsers
let pint = pint32 |>> Int .>> ws <!> "pint"

let pbool =
    (stringReturn "true" (Bool true) <|> stringReturn "false" (Bool false)) .>> ws <!> "pbool"

// Identifier parser
let isIdentifierFirstChar c = isLetter c || c = '_'
let isIdentifierChar c = isLetter c || isDigit c || c = '_'
let identifier = 
    many1Satisfy2 isIdentifierFirstChar isIdentifierChar .>> ws <!> "identifier"

// Variable reference parser
let variable = identifier |>> Var <!> "variable"

// String literal parser
let stringLiteral = 
    between (pstring "\"") (pstring "\"") (manyChars (noneOf "\"")) .>> ws <!> "stringLiteral"

// Comment parser - headings are used as comments
let headingComment =
    let headingPrefix = many1 (pchar '#') .>> pchar ' '
    headingPrefix >>. restOfLine true |>> Comment <!> "headingComment"

// Variable declaration: - variable = value
// Variable declaration: - variable = value
let variableDeclaration =
    let dashIdentifier = pstring "- " >>. identifier
    dashIdentifier .>> str_ws "=" .>>. expr .>> ws .>> optional skipNewline
    |>> (fun (name, value) -> Let(name, value, Var name)) <!> "variableDeclaration"

// Function arguments parser
let identifierNoWs = many1Satisfy2 isIdentifierFirstChar isIdentifierChar <!> "identifierNoWs"

let functionArgs = 
    sepBy (identifierNoWs .>> ws) (pchar ',' >>. ws) <!> "functionArgs"

// Conditional: if condition: [x] true_branch [ ] false_branch
let conditional, conditionalRef = createParserForwardedToRef<expr, unit>()

// Define the conditional parser
conditionalRef.Value <-
    str_ws "if" >>. expr .>> pchar ':' .>> ws .>> optional skipNewline .>>.
    many1 (ws >>. str_ws "[x]" >>. expr .>> optional skipNewline) .>>.
    many (ws >>. str_ws "[ ]" >>. expr .>> optional skipNewline)
    |>> (fun ((condition, trueExprs), falseExprs) -> 
        let trueExpr = 
            match trueExprs with
            | [single] -> single
            | multiple -> // Create sequence of expressions using Let with "_"
                List.fold (fun expr nextExpr -> Let("_", expr, nextExpr))
                          (List.head multiple) 
                          (List.tail multiple)

        let falseExpr = 
            match falseExprs with
            | [] -> Var "()"
            | [single] -> single
            | multiple -> // Create sequence of expressions using Let with "_"
                List.fold (fun expr nextExpr -> Let("_", expr, nextExpr))
                          (List.head multiple) 
                          (List.tail multiple)

        Cond(condition, trueExpr, falseExpr)) <!> "conditional"

// Function body conditional parser - specifically for conditionals within function bodies
let functionBodyConditional =
    str_ws "if" >>. expr .>> pchar ':' .>> ws .>> optional skipNewline .>>.
    many1 (ws >>. str_ws "> [x]" >>. expr .>> optional skipNewline) .>>.
    many (ws >>. str_ws "> [ ]" >>. expr .>> optional skipNewline)
    |>> (fun ((condition, trueExprs), falseExprs) -> 
        let trueExpr = 
            match trueExprs with
            | [single] -> single
            | multiple -> // Create sequence of expressions using Let with "_"
                List.fold (fun expr nextExpr -> Let("_", expr, nextExpr))
                          (List.head multiple) 
                          (List.tail multiple)

        let falseExpr = 
            match falseExprs with
            | [] -> Var "()"
            | [single] -> single
            | multiple -> // Create sequence of expressions using Let with "_"
                List.fold (fun expr nextExpr -> Let("_", expr, nextExpr))
                          (List.head multiple) 
                          (List.tail multiple)

        Cond(condition, trueExpr, falseExpr)) <!> "functionBodyConditional"

// Function body line parser - handles conditionals within function bodies
let functionBodyLine =
    choice [
        attempt (pstring "> " >>. functionBodyConditional)
        attempt (pstring "> ^" >>. ws >>. expr) // Special case for return expression
        pstring "> " >>. expr
    ] .>> ws .>> optional skipNewline <!> "functionBodyLine"

// Lambda function definition: >[!] arg1, arg2, ...
let lambdaFunctionDefinition =
    str_ws ">[!]" >>. 
    functionArgs .>> ws .>> optional skipNewline .>>.
    many functionBodyLine
    |>> (fun (args, bodyLines) ->
        // Create nested lambda expressions for multiple arguments
        let rec buildLambda args body =
            match args with
            | [] -> body
            | arg::rest -> Lam(arg, buildLambda rest body)
        
        // Process body lines to find return expression (^)
        let bodyExpr, returnExpr =
            let lastIndex = bodyLines.Length - 1
            if lastIndex >= 0 then
                match bodyLines.[lastIndex] with
                | App(Var "^", value) -> 
                    // Return expression found
                    let bodyLines' = if lastIndex > 0 then bodyLines.[0..lastIndex-1] else []
                    let bodyExpr = 
                        match bodyLines' with
                        | [] -> Var "()"
                        | [single] -> single
                        | _ -> List.fold (fun acc next -> Let("_", acc, next)) bodyLines'.[0] bodyLines'.[1..]
                    bodyExpr, Some value
                | _ ->
                    // No return expression
                    let bodyExpr = 
                        match bodyLines with
                        | [] -> Var "()"
                        | [single] -> single
                        | _ -> List.fold (fun acc next -> Let("_", acc, next)) bodyLines.[0] bodyLines.[1..]
                    bodyExpr, None
            else
                Var "()", None
        
        // Create the lambda expression
        let lambdaExpr = buildLambda args bodyExpr
        
        // If there's a return expression, apply the lambda to it
        match returnExpr with
        | Some value -> App(lambdaExpr, value)
        | None -> lambdaExpr) <!> "lambdaFunctionDefinition"

// Regular function definition: >[!function_name] arg1, arg2, ...
// This is now only used for parsing, but not included in the top-level declarations
let functionDefinition =
    str_ws ">[!" >>. identifier .>> str_ws "]" .>>.
    functionArgs .>> ws .>> optional skipNewline .>>.
    many1 functionBodyLine
    |>> (fun ((name, args), body) ->
        // Create nested lambda expressions for multiple arguments
        let rec buildLambda args body =
            match args with
            | [] -> body
            | arg::rest -> Lam(arg, buildLambda rest body)
        
        // Combine multiple expressions into a single expression using Let with "_" variable
        let combinedBody =
            match body with
            | [single] -> single
            | multiple -> 
                // Use nested Let expressions with "_" instead of PFunc ";"
                List.fold (fun expr nextExpr -> Let("_", expr, nextExpr)) 
                          (List.head multiple) 
                          (List.tail multiple)
        
        Let(name, buildLambda args combinedBody, Var name)) <!> "functionDefinition"

// Named function declaration: - >[!function_name] arg1, arg2, ...
let namedFunctionDeclaration =
    let dashPrefix = pstring "- " <!> "dashPrefix"
    dashPrefix >>. 
    (str_ws ">[!" >>. identifier .>> str_ws "]" .>>.
     functionArgs .>> ws .>> optional skipNewline .>>.
     many1 functionBodyLine)
    |>> (fun ((name, args), body) ->
            // Create nested lambda expressions for multiple arguments
            let rec buildLambda args body =
                match args with
                | [] -> body
                | arg::rest -> Lam(arg, buildLambda rest body)
            
            // If you want to combine multiple expressions in the body
            let combinedBody = 
                match body with
                | [single] -> single
                | multiple -> 
                    // Use nested Let expressions with "_" instead of PFunc ";"
                    List.fold (fun expr nextExpr -> Let("_", expr, nextExpr))
                             (List.head multiple)
                             (List.tail multiple)
                             
            Let(name, buildLambda args combinedBody, Var name)) <!> "namedFunctionDeclaration"

// Named recursive function declaration: - >[!!function_name] arg1, arg2, ...
let namedRecursiveFunctionDeclaration =
    let dashPrefix = pstring "- " <!> "dashPrefix"
    dashPrefix >>. 
    (str_ws ">[!!" >>. identifier .>> str_ws "]" .>>.
     functionArgs .>> ws .>> optional skipNewline .>>.
     many1 functionBodyLine)
    |>> (fun ((name, args), body) ->
            // Create nested lambda expressions for multiple arguments
            let rec buildLambda args body =
                match args with
                | [] -> body
                | arg::rest -> Lam(arg, buildLambda rest body)
            
            // If you want to combine multiple expressions in the body
            let combinedBody = 
                match body with
                | [single] -> single
                | multiple -> 
                    // Use nested Let expressions with "_" instead of PFunc ";"
                    List.fold (fun expr nextExpr -> Let("_", expr, nextExpr))
                             (List.head multiple)
                             (List.tail multiple)
                             
            LetRec(name, buildLambda args combinedBody, Var name)) <!> "namedRecursiveFunctionDeclaration"
    
// Function call: [function_name](arg1, arg2, ...)
let functionCall =
    between (str_ws "[") (str_ws "]") identifier .>>.
    between (str_ws "(") (str_ws ")") (sepBy expr (str_ws ","))
    |>> (fun (name, args) ->
        // Apply arguments one by one
        List.fold (fun acc arg -> App(acc, arg)) (Var name) args) <!> "functionCall"
 
let stringExpr = 
    stringLiteral |>> Str <!> "stringExpr"


// File operations
// read_file "foo.txt"
let readFile =
    str_ws "[read_file]" >>.
    between (str_ws "(") (str_ws ")") stringLiteral
    |>> (fun path -> ReadFile(Str path)) <!> "readFile"

// write_file "out.txt", expr
let writeFile =
    str_ws "[write_file]" >>.
    between (str_ws "(") (str_ws ")")
        (stringLiteral .>> str_ws "," .>>. expr)
    |>> (fun (path, content) -> WriteFile(Str path, content)) <!> "writeFile"

// List: [1, 2, 3, 4]
let listExpr =
    between (str_ws "[") (str_ws "]") (sepBy expr (str_ws ","))
    |>> (fun xs ->                               // [e1, e2, e3] →
        List.foldBack (fun h t -> Cons(h,t)) xs Nil) <!> "list"

// Range: [1..10]
let range =
    between (str_ws "[") (str_ws "]") (expr .>> str_ws ".." .>>. expr)
    |>> Range <!> "range"

// Output: ! value
// Modified to accept only one expression and convert it to a print function call
let output =
    str_ws "!" >>. expr
    |>> (fun e -> App(PFunc "print", e)) <!> "output"

// Term parser (for basic expressions)
let term =
    choice [
        pint
        pbool
        attempt functionCall
        variable
        attempt listExpr
        attempt range
        attempt readFile
        attempt writeFile
        attempt conditional
        attempt output
        attempt stringExpr
        between (str_ws "(") (str_ws ")") expr
    ] <!> "term"

// Binary operations parser
let opp = OperatorPrecedenceParser<expr, unit, unit>()
opp.TermParser <- term

// Define operators with appropriate precedence
let addOp = InfixOperator("+", ws, 1, Associativity.Left, 
                         fun x y -> App(App(PFunc "+", x), y))
let subOp = InfixOperator("-", notFollowedBy (pchar ' ' >>. identifier) >>. ws, 1, 
                      Associativity.Left, fun x y -> App(App(PFunc "-", x), y))

let mulOp = InfixOperator("*", ws, 2, Associativity.Left, 
                         fun x y -> App(App(PFunc "*", x), y))
let divOp = InfixOperator("/", ws, 2, Associativity.Left, 
                         fun x y -> App(App(PFunc "/", x), y))
let remOp = InfixOperator("%", ws, 2, Associativity.Left, 
                         fun x y -> App(App(PFunc "%", x), y))
let eqOp = InfixOperator("=", ws, 1, Associativity.None, 
                        fun x y -> App(App(PFunc "=", x), y))

// Modify the greater-than operator to not match at the beginning of a line
// or when it's part of a function body line pattern
let gtOp = InfixOperator(">", 
                    notFollowedBy (pchar ' ') >>. 
                    notFollowedBy (pstring "[!") >>. 
                    ws, 
                    1, Associativity.None, 
                    fun x y -> App(App(PFunc ">", x), y))

let ltOp = InfixOperator("<", ws, 1, Associativity.None, 
                        fun x y -> App(App(PFunc "<", x), y))
let gteOp = InfixOperator(">=", ws, 1, Associativity.None, 
                         fun x y -> App(App(PFunc ">=", x), y))
let lteOp = InfixOperator("<=", ws, 1, Associativity.None, 
                         fun x y -> App(App(PFunc "<=", x), y))

// Logical operators
let andOp = InfixOperator("and", ws, 1, Associativity.Left, 
                         fun x y -> App(App(PFunc "and", x), y))
let orOp = InfixOperator("or", ws, 1, Associativity.Left, 
                        fun x y -> App(App(PFunc "or", x), y))

let htOp = InfixOperator(":-:", ws, 3, Associativity.Right,
                         fun h t -> Cons(h,t))

// Add operators to the parser
opp.AddOperator(addOp)
opp.AddOperator(subOp)
opp.AddOperator(mulOp)
opp.AddOperator(divOp)
opp.AddOperator(remOp)
opp.AddOperator(eqOp)
opp.AddOperator(gtOp)
opp.AddOperator(ltOp)
opp.AddOperator(gteOp)
opp.AddOperator(lteOp)
opp.AddOperator(andOp)
opp.AddOperator(orOp)
opp.AddOperator(htOp)

// Connect the expression reference to our operator parser
exprRef.Value <- opp.ExpressionParser <!> "expr"

// Top-level constructs
let declaration =
    choice [
        attempt namedFunctionDeclaration
        attempt namedRecursiveFunctionDeclaration
        attempt variableDeclaration
    ] <!> "declaration"

// Program parser - a series of declarations and expressions
let program = 
    let emptyLine = skipNewline >>% Comment ""

    // Make each parser definition handle its own newlines completely
    let lineParser = 
        choice [
            attempt (ws >>. headingComment .>> optional skipNewline)
            attempt (ws >>. output .>> optional skipNewline)  
            attempt (ws >>. conditional .>> optional skipNewline)
            attempt (ws >>. namedFunctionDeclaration .>> optional skipNewline)
            attempt (ws >>. namedRecursiveFunctionDeclaration .>> optional skipNewline)
            attempt (ws >>. variableDeclaration .>> optional skipNewline)
            attempt (ws >>. lambdaFunctionDefinition .>> optional skipNewline)
            attempt (ws >>. expr .>> optional skipNewline)
            emptyLine  // Already handles its own newline
        ]

    many lineParser .>> eof <!> "program"

        
// Function to parse MD# code
let parseProgram (code: string) =
    match run program code with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwith errorMsg