# 🧪 MD-Sharp

An experimental interpreted programming language implemented in F#.

The project focuses on building a full execution pipeline — from parsing source code to evaluating it via a custom interpreter.



## 🚀 Features

* Custom language syntax
* Parser based on FParsec (parser combinators)
* Abstract Syntax Tree (AST)
* Interpreter with environment model
* Variables and expressions
* Recursive functions
* Conditional expressions
* Lists and ranges
* Basic file I/O



## 🧠 Architecture

```text
Source Code → Parser → AST → Evaluator
```

* **Parser** transforms source code into a structured AST
* **AST** represents the program as composable expressions
* **Evaluator** executes expressions using an environment



## ⚙️ Example

```md
- >[!!fact] n
  > if n <= 1:
  > [x] 1
  > [ ] n * [fact](n - 1)

- n = 6
! [fact](n)
```



## 💡 Key Ideas

* Functional parsing using combinators (FParsec)
* Clear separation between parsing and execution
* Support for recursion via environment-based evaluation
* Expression-oriented design



## 🎯 Purpose

* Understand how interpreters work internally
* Practice AST design and evaluation strategies
* Explore language design concepts



## 🛠 Tech Stack

* F#
* FParsec



## 📌 Future Work

* Type system
* Better error handling
* REPL
* Modules and imports



## 📫 Author

Denis Avramenko
