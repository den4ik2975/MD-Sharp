# MD# language documentation
## Basic Concepts
MD# is a functional programming language with a syntax based on Markdown.

Our idea was to make a programming language wil markdown formating. So, you can write your code in .md files and pass them to the interpreter. In mardown viewers you will be able to see your code with markdown formatting.
## Syntax
### Variables and constants
Named variable (let in F#) declaration is performed using a bulleted list:

`- variable = expresion`
### Functions
#### Anonymous (lambda) functions
The lambda function declaration uses the callout syntax from Markdown:
```
>[!] argument1, argument2, ...
> function body
> continuation of the function body
> return_value
> ^arg1, arg2, ...
```
For example:
```
> [!] x
> x * x
> ^4
```
#### Named functions
For named functions (like let fun in F#) you should use named variables. Name is specified inside the square brackets
```
- >[!function_name] arg1, arg2...
  > function body
```

P.s. This was inspired by Obsidian Callouts

**Double whitespace for func body are required**

Recursive functions should have double ! in their names:
`- >[!!recursive_function_name] arg1, arg2, ...`
#### Call
The function call uses the syntax of references:
`[function_name](argument1, argument2, ...)`



### Conditional constructions

Conditional expressions use checkboxes:
```
if condition:
[x] code if condition is true
[x] also code if condition is true
[ ] code if the condition is false
```
Nested conditions (under dev):
```
if condition1:
[x] code for true condition1
[x] if condition2:
[x] [x] code for true conditions 1 and 2
[x] [ ] code for true condition1 and false condition2
[ ] code for a false condition1
```

P.S. The idea was to create a to-do list and then the truth branch would be ticked. But to create a to-do list, you also need to use - (bulleted list). We thought that would be too much, so we left it that way. But by using the checkboxes, you can understand at which stage of each of the nested conditions you are (true/false)
### Input/Output
Console output:
`! expr`

P.s. This syntax was inspired by image show syntax from markdown `![]()`:  `! [func](arg1, arg2, ...)`

Working with files (under dev):
```
- content = [read_file]("путь/к/файлу.txt ")
  [write_file]("путь/к/выходному_файлу.txt ", content)
```
### Lists and sequences
Declaring Lists:
`- list_name = [1, 2, 3, 4]`
`- range_name = [1..10]`

Operations with lists:
head\tail
`h:-:t`

### Comments and documentation

Headings are used as comments to structure the code:
```
# Main section
## Subsection
### Functionality
```

### Examples
You can find more examples in samples directory
#### Factorial calculation (recursion)
```
# Factorial calculation function

>[!!factorial] n
> if n <= 1:
> [x] 1
> [ ] n * [factorial](n-1)
```

#### Adding function
```
# Sample Program

- x = (5 + 5 - 2) * 3
- y = 10

- >[!add] a, b
  > a + b

! [add](x, y)
```