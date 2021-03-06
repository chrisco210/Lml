%{
open Ast
%}
 /* Immidiate values  */
%token <int> INT
%token <string> ID
%token UNIT
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

%token WHILE
%token DO
%token DONE

%token BREAK
%token CONTINUE

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
%token DIV

%token NOT
%token NEG
%token HD
%token TL

%token NIL
%token IS_NIL

/* References */
%token COLONEQUALS
%token DEREF
%token REF

%token GET
%token SET

/* (*Function things*) */
%token FUN 
%token ARROW
%token LAMBDA
%token APP

%token EOF

%nonassoc PERIOD ELSE IN ARROW

%left SEMICOLON

%left COLONEQUALS

%left AND
%left OR
%left LTEQ
%left GTEQ
%left GT
%left LT
%left NEQ
%left EQUALS
%left DIV
%left PLUS
%left MINUS
%left TIMES
%right CONS

/* Thanks to https://ptival.github.io/2017/05/16/parser-generators-and-function-application/
  for how to make function application left associative
 */
%nonassoc LAMBDA  IF WHILE LET LETREC LPAREN FUN ID INT TRUE FALSE NOT NEG HD TL NIL UNIT CONTINUE BREAK REF  IS_NIL GET SET
%nonassoc DEREF
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
  | DIV { Div }
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
  | b = UNIT { Unit }
  | l = NIL { Nil }
  | IS_NIL e = expr {IsNil(e)}
  | BREAK {Break}
  | CONTINUE {Continue}

  | REF e = expr {Ref e}
  | DEREF e = expr {Deref e}
  | e1 = expr COLONEQUALS e2 = expr {Assign(e1, e2)}
  | GET { Get }
  | SET e = expr { Set(e) }

  | e1 = expr SEMICOLON e2 = expr {Seq (e1, e2)}
  | LAMBDA v = ID PERIOD e = expr {Abs (v, e)}
  | LET v = ID EQUALS e1 = expr IN e2 = expr {Let (v, e1, e2)}
  | LETREC v = ID EQUALS FUN a = fun_args ARROW e = expr IN e1 = expr{Letrec (v, a, e, e1)} 
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr {If (e1, e2, e3)}
  | WHILE g = expr DO e = expr DONE {While(g, e)}
  | FUN a = fun_args ARROW e = expr {Fun (a, e)}
  | e1 = expr e2 = expr %prec APP {App (e1, e2)}
  | e1 = expr b = binop e2 = expr {Bop (e1, b, e2)}
  | u = uop e = expr {Uop (u,e)}
  | e1 = expr POUND n = INT { Proj(e1, n) }
  | LPAREN e=expr RPAREN {e}
  | LPAREN e = tuple_args RPAREN {Tuple e}
  ;
fun_args:
  | v1 = ID { [v1] }
  | h = ID t = fun_args { h::t }
  ;
tuple_args:
  | v1 = expr COMMA v2 = expr { [v1; v2] }
  | h = expr COMMA t = tuple_args { h::t }
  ;