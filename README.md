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

You can run `lml raw` to start a REPL interpreter that directly converts lambda
calculus expressions into De Bruijn notation without converting to CPS and state
passing style.  

## Sample Programs

In the examples folder, there are a number of example programs featuring all
of the features.  

## The ML language

The ML language is defined with the following grammar:

```
bop ::= + | - | * | / | < | > | <= | >= | = | != | :: | && | || 
uop ::= ~ | ~- | !

e ::= (e)
      | let x = e1 in e2
      | let rec x = e1 in e2
      | fun x1 ... xn -> e
      | e1 e2
      | if e1 then e2 else e3
      | while e1 do e2 done
      | break
      | continue
      | e1; e2
      | ref e
      | e1 := e2
      | get
      | set e
      | x
      | []                   <-- Nil
      | L x . e
      | (e1, e2, ... en)
      | e#n
      | e1 bop e2
      | uop e
      | n
      | ()                   <-- Unit
      | true
      | false
```

These generally do what you would expect them to do.  The `::` operator is a 
cons operator to append to a list, `e#n` extracts the `n`th item from a tuple
`e`. For unary operators,  `~` is logical negation, while `~-` is integer 
negation, and `!` is dereferencing.

You can do OCaml style comments using the `(* ocaml comment *)` syntax.

One note is you cannot have blank lines in your written programs.

## Target Lambda calculus
The target lambda calculus is De Bruijn lambda calculus extended with integers,
booleans, operators, and if statements:

```
e ::= x
      | n
      | b
      | ()
      | $i <-- This is a De Bruijn variable number
      | if e1 then e2 else e3
      | L . e
      | e1 e2
      | if e1 then e2 else e3
      | e1 bop e2
      | uop e
bop ::= + | - | * | / | < | > | <= | >= | = | !=
uop ::= ~-
```

The lambda calculus is evaluated using big step call by name (lazy) evaluation.