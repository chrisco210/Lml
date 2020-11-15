# Lml
A compiler for an ML-style language that compiles to lambda calculus.  
CS4110 Final Project

This takes an ML-style language with imperative features, and compiles it into
lambda calculus extended with integers, the usual set of arithmetic and 
comparison operators, and if statements.  

## Build

The interpreter uses the menhir parser and ocamllex to generate parsers and 
lexers, so you will need to install both.  

Type `make` to build and run the program, which by default launches a repl
interface.

Type `make build` to build the binary without launching.  It outputs to 
`main.byte`.

Type `make test` to run tests.

## Usage

Running `lml [inputfile]` will parse the program in the input file and output
the resulting lambda calculus and the result.

Running just `lml` will start a simple REPL interpreter to use interactively.
Right now it is pretty simple, but we will work on improving it in the future
sprints.

## Sample Programs

Here are some sample programs to run:

Factorial function using the Y combinator and lambda calculus notation:
```
let Y = L f . (L x . f (x x)) (L x . f (x x)) in
let G = L f . L n . if n = 0 then 1 else n * (f (n - 1)) in
let factorial = Y G in
factorial 4
```
This evaluates to 4! = 24

Functions keep their environments, as you would expect:
```
let x = 4 in
let f = fun z -> z + 4 in
let x = 10 in
f 6
```
This evaluates to 10

Functions can have any number of arguments:
```
let add = fun x y -> x + y in
add 1 2
```
This evaluates to 3

Partial application also works:
```
let add = fun x y -> x + y in
let addtwo = add 2 in
addtwo 2
```
This evaluates to 4

Functions are values, and will halt evaluation:
```
let x = fun a -> a in
x
```
Will evaluate to `L . $0`, which is the identity function in De Bruijn notation.

You can also write explicit recursive functions.  The same factorial function
as above can be written as:
```
let rec fact = fun n -> if n = 0 then 1 else n * fact (n - 1) in
fact 4
```
Evaluates to 24 again
## The ML language

The ML language is defined with the following grammar:

```
bop ::= + | - | * | < | > | <= | >= | = | != | ::

e ::= (e)
      | let x = e1 in e2
      | fun x1 ... xn -> e
      | e1 e2
      | if e1 then e2 else e3
      | while e1 do e2
      | break
      | continue
      | e1; e2
      | ref e
      | e1 := e2
      | x
      | Nil
      | L x . e
      | let x = e
      | (e1, e2, ... en)
      | e#n
      | e1 bop e2
      | n
      | true
      | false
```

These generally do what you would expect them to do.  The `::` operator is a 
cons operator to append to a list, `e#n` extracts the `n`th item from a tuple
`e`

## Target Lambda calculus
The target lambda calculus is De Bruijn lambda calculus extended with integers,
booleans, operators, and if statements:

```
e ::= x
      | n
      | b
      | i <-- This is a De Bruijn variable number
      | if e1 then e2 else e3
      | L . e
      | e1 e2
```

The lambda calculus is evaluated using big step call by name (lazy) evaluation.