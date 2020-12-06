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
%token COMMA
%token CONS

%token SEMICOLON

%token POUND

/* Let expressions */
%token LET
%token IN
%token LETREC

/* (*If statements*) */
%token IF
%token THEN
%token ELSE


/* (*Operators*) */
%token AND
%token OR

%token LTEQ
%token GTEQ
%token LT
%token GT

%token TIMES  
%token PLUS
%token MINUS
%token EQUALS
%token NEQ

%token NOT
%token NEG
%token HD
%token TL

%token NIL

/* (*Function things*) */
%token FUN 
%token ARROW
%token LAMBDA
%token APP

%token EOF

%nonassoc PERIOD ELSE IN ARROW 

%left AND
%left OR
%left LTEQ
%left GTEQ
%left GT
%left LT
%left NEQ
%left EQUALS
%left PLUS
%left MINUS
%left TIMES
%right CONS
%left SEMICOLON
/* Thanks to https://ptival.github.io/2017/05/16/parser-generators-and-function-application/
  for how to make function application left associative
 */
%nonassoc LAMBDA IF LET LETREC LPAREN FUN ID INT TRUE FALSE NOT NEG HD TL NIL

%nonassoc APP

%nonassoc POUND

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
  | AND { And }
  | OR { Or }
  | CONS { Cons }
  ;
%inline uop:
  | NOT { Not }
  | NEG { Neg }
  | TL { Tl }
  | HD { Hd }
  ;
prog: 
  | e = expr; EOF { e }
  ;
expr:
  | i = INT { Int i }
  | x = ID { Var x }
  | b = TRUE { Bool true }
  | b = FALSE { Bool false }
  | l = NIL { Nil }
  | e1 = expr SEMICOLON e2 = expr {Seq (e1, e2)}
  | LAMBDA v = ID PERIOD e = expr {Abs (v, e)}
  | LET v = ID EQUALS e1 = expr IN e2 = expr {Let (v, e1, e2)}
  | LETREC v = ID EQUALS FUN a = fun_args ARROW e = expr IN e1 = expr{Letrec (v, a, e, e1)} 
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr {If (e1, e2, e3)}
  | FUN a = fun_args ARROW e = expr {Fun (a, e)}
  | e1 = expr e2 = expr %prec APP {App (e1, e2)}
  | e1 = expr b = binop e2 = expr {Bop (e1, b, e2)}
  | u = uop e = expr {Uop (u,e)}
  | e1 = expr POUND n = INT { Proj(e1, n) }
  | LPAREN e=expr RPAREN {e}
  | LPAREN e1 = expr COMMA e2 = expr RPAREN {Tuple (e1, e2)}
  ;
fun_args:
  | v1 = ID { [v1] }
  | h = ID t = fun_args { h::t }