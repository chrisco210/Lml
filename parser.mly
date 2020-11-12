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
%token NUMSIGN
%token PERIOD
%token COMMA


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
%token NOT
%token NEG

/* (*Function things*) */
%token FUN 
%token ARROW
%token LAMBDA

%token EOF

%nonassoc IN
%nonassoc ELSE

%left LTEQ
%left GTEQ
%left GT
%left LT
%left MINUS
%left EQUALS
%left NEQ
%left PLUS
%left TIMES

%start <Ast.expr> prog