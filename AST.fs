module AST

type id = string

// объявляем env сразу, чтобы использовать его в Expr
type env = Map<id, expr>

and expr =
| App      of expr * expr
| Lam      of id   * expr
| Var      of id
| Int      of int
| Str      of string
| Bool     of bool
| PFunc    of id
| Cond     of expr * expr * expr
| Let      of id * expr * expr
| LetRec   of id * expr * expr
| Op       of id * int * expr list
// списки
| Nil
| Cons     of expr * expr
// замыкания
| Closure  of expr * env
| RClosure of expr * env * id
// файлы
| ReadFile  of expr                    // путь ‒ выражение-строка
| WriteFile of expr * expr             // путь, содержимое
| Range    of expr * expr
| Comment  of string