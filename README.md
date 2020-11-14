# Lml
A compiler for an ML-style language that compiles to lambda calculus.  
CS4110 Final Project

This takes an ML-style language with imperative features, and compiles it into
lambda calculus extended with integers, the usual set of arithmetic and 
comparison operators, and if statements.  

## Usage
`lml [inputfile]`

Running the program with no arguments will launch a repl interface which will
evaluate expressions you type. Options is one of the following:
```
--output <file>             Places the resulting LC into <file>
--noexec                    Compiles LC without executing it
```

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