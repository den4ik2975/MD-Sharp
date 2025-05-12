# MD language documentation
## Basic Concepts
MD# is a functional programming language with a syntax based on Markdown. The language is designed for maximum readability and ease of parsing.
## Syntax
### Variables and constants
Variable (let in F#) declaration is performed using a bulleted list:
`- variable = value of the var`
### Functions
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
For named functions (like let fun in F#) are defined like variables:
```
- >[!function_name] arg1, arg2...
  > function body
```
Recursive functions should have double !! in their names:
`- >[!!recursive_function_name] arg1, arg2, ...`
The function call uses the syntax of references:
`[function_name](argument1, argument2, ...)`

**Two whitespaces for func body are required**

### Conditional constructions
Conditional expressions use checkboxes:
```
if condition:
[x] code if condition is true
[ ] code if the condition is false
```
Nested conditions:
```
if condition1:
[x] code for true condition1
[x] if condition2:
[x] [x] code for true conditions 1 and 2
[x] [ ] code for true condition1 and false condition2
[ ] code for a false condition1
```
### Input/Output
Console output:
`! value1, value2, ...`

Working with files:
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