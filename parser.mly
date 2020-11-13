%{
open Ast
%}
 /* Immidiate values  */
%token <int> INT
%token <string> ID
%token TRUE
%token FALSE

/* Random syntax stuff */
%token LPAREN
%token RPAREN
/* %token NUMSIGN */
%token PERIOD
/* %token COMMA */


/* Let expressions */
/* %token LET */
/* %token IN */

/* (*If statements*) */
/* %token IF */
/* %token THEN */
/* %token ELSE */

/* (*WHile stuff*) */
/* %token WHILE
%token DO
%token BREAK
%token CONTINUE */

/* (*References*) */
/* %token ASSIGN
%token REF 
%token BANG */

/* (*Sequencing*) */
/* %token SEMICOLON */

/* (*List operations*) */
/* %token NIL
%token CONS  */



/* (*Operators*) */
%token LTEQ
%token GTEQ
%token LT
%token GT

%token TIMES  
%token PLUS
%token MINUS
%token EQUALS
%token NEQ

/* (*Unary Operators*) */
/* %token NOT
%token NEG */

/* (*Function things*) */
/* %token FUN  */
/* %token ARROW */
%token LAMBDA

%token EOF

/* %nonassoc IN
%nonassoc ELSE */

%nonassoc LAMBDA
%nonassoc PERIOD

%left LTEQ
%left GTEQ
%left GT
%left LT
%left NEQ
%left EQUALS
%left MINUS
%left PLUS
%left TIMES


%start <Ast.expr> prog

%%
prog: 
  | e = expr; EOF { e }
  ;
expr:
/* Literals */
  | i = INT { Int i }
  | x = ID { Var x }
  | b = TRUE { Bool true }
  | b = FALSE { Bool false }
/* Functions */
  | e1 = expr; e2 = expr {App (e1, e2)}
  | LAMBDA; v = ID; PERIOD; e = expr {Abs (v, e)}
/* Binops */
  | e1 = expr; LTEQ; e2 = expr {Bop (e1, Lteq, e2)}
  | e1 = expr; GTEQ; e2 = expr {Bop (e1, Gteq, e2)}
  | e1 = expr; GT; e2 = expr {Bop (e1, Gt, e2)}
  | e1 = expr; LT; e2 = expr {Bop (e1, Lt, e2)}
  | e1 = expr; MINUS; e2 = expr {Bop (e1, Minus, e2)}
  | e1 = expr; PLUS; e2 = expr {Bop (e1, Plus, e2)}
  | e1 = expr; TIMES; e2 = expr {Bop (e1, Times, e2)}
  | e1 = expr; NEQ; e2 = expr {Bop (e1, Neq, e2)}
  | e1 = expr; EQUALS; e2 = expr {Bop (e1, Equals, e2)}
  | LPAREN; e=expr; RPAREN {e}
  ;