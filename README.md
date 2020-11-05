# Lml
A compiler for an ML-style language that compiles to lambda calculus.  CS4110 Final Project

## Grammar
The grammar is defined as follows:
```
exp ::=
| n
| e1 + e2 | e1 * e2 | e1 - e2 | e1 = e2 | e1 < e2 | e1 >= e2
| let x = e1 in e2
| function (x1 x2 ... xn) -> e
| function rec (x1 x2 ... xn) -> e
| if e1 then e2 else e3 
```

We compile the lambda calculus to the following language, which is lambda calculus extended with natural numbers
```
exp ::= 
| e1 e2
| Lambda x . e
| x 
| n
```

### Grammar
