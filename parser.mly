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
%token LET
%token IN

/* (*If statements*) */
%token IF
%token THEN
%token ELSE

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

%nonassoc PERIOD
%nonassoc LAMBDA 
%nonassoc IN


%nonassoc ELSE
%nonassoc THEN
%nonassoc IF

%left LTEQ
%left GTEQ
%left GT
%left LT
%left NEQ
%left EQUALS
%left PLUS
%left MINUS
%left TIMES


%nonassoc TRUE FALSE
%nonassoc INT
%nonassoc ID

%nonassoc LPAREN


%start <Ast.expr> prog

%%
%inline binop:
  | LTEQ { Lteq }
  | GTEQ { Gteq }
  | GT { Gt }
  | LT { Lt }
  | MINUS { Minus }
  | PLUS { Plus }
  | TIMES { Times }
  | NEQ { Neq }
  | EQUALS { Equals }
  ;
prog: 
  | e = expr; EOF { e }
  ;
expr:
  | i = INT { Int i }
  | x = ID { Var x }
  | b = TRUE { Bool true }
  | b = FALSE { Bool false }
  | LAMBDA v = ID PERIOD e = expr {Abs (v, e)}
  | LET v = ID EQUALS e1 = expr IN e2 = expr {Let (v, e1, e2)}
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr {If (e1, e2, e3)}
  | e1 = expr e2 = expr {App (e1, e2)}
  | e1 = expr b = binop e2 = expr {Bop (e1, b, e2)}
  | LPAREN e=expr RPAREN {e}
  ;
