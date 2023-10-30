
/* Analyseur syntaxique pour Arith */

%{
  open Ast
%}

%token <string> CST
%token <string> IDENT
%token RETURN IF ELSE
%token EOF
%token LP RP LBRACE RBRACE LB RB
%token PLUS MINUS TIMES DIV MOD
%token COMMA SEMICOLON
%token AND OR
%token EQ EQQ
%token LEQ GEQ LE GE NEQ
%token NOT PTR
%token INT VOID

/* Definitions des priorites et associativites des tokens */

%right EQ
%left OR AND
%nonassoc EQQ NEQ
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc uminus
%nonassoc utimes

/* Poin
t d'entree de la grammaire */
%start prog

/* Type des valeurs retournees par l'analyseur syntaxique */
%type <Ast.cprogram> prog

%%

dtype:
| INT  { Dint }
| VOID  { Dvoid }
prog:
| p = list(def) EOF { p }
;

suite: b = simple_stmt ; SEMICOLON { b }
  | s = stmt+ ; { Sblock(s),$startpos }
;

def :
| ret = dtype ; s = IDENT; LP ; args = separated_list(COMMA,IDENT) ; RP ; SEMICOLON {{ret_type = ret ; name = s ; args = args ; body = None }}
| ret = dtype ; s = IDENT ; LP ; args = separated_list(COMMA,IDENT) ; RP ; LBRACE ; b = suite ; RBRACE
{{ret_type = ret ; name = s ; args = args ; body = Some (b) }}

left_value:
| s = IDENT { Var(s) }

simple_stmt:
  | RETURN ; e = expr { Sreturn(e), $startpos }
  | ret = dtype; l = left_value {Sdeclarevar(ret, l), $startpos}
  | l = left_value ; EQ ; e = expr { Sassign(l,e), $startpos }
  // | ret = TYPE; l = left_value ; EQ ; e = expr ; { Sinitvar (ret, l ,e), $startpos}
  | e = expr { Sval(e), $startpos }
;

stmt:
| s = simple_stmt ; SEMICOLON  { s }
// | FOR ; s = IDENT ; IN ; e = expr ; COLON ; b = suite  {Sfor(s,e,b), $startpos}
// | IF ; cond = expr ; COLON ; b = suite  {Sif(cond, b), $startpos}
// | IF ; cond = expr ; COLON ; b = suite ; ELSE ; COLON ; e = suite  {Sifelse(cond,b,e), $startpos}
// | WHILE ; cond = expr ; COLON; b = suite {Swhile(cond,b), $startpos}
;

expr:
| c = const                      { Const(c) }
| l = left_value                 { Val(l)}
| e1 = expr o = op e2 = expr     { Op(o,e1,e2) }
| MINUS e = expr %prec uminus    { Moins(e) }
// | PTR e = INDENT                 { Ptr(e) }
// | TIMES e = expr %prec uminus    {  Star(e) }
| NOT e = expr                   { Not(e) }
| s = IDENT ; LP ; args = separated_list(COMMA,expr) ; RP { Ecall(s,args) }
// | LB ; args = separated_list(COMMA,expr) ; RB { List(args)} array plutot
| LP ; e = expr ; RP { e }

;

%inline op:
| PLUS  { Add}
| MINUS { Sub }
| TIMES { Mul }
| DIV   { Div }
| MOD   { Mod }
| LEQ   { Leq }
| GEQ   { Geq }
| GE    { Ge  }
| LE    { Le  }
| NEQ   { Neq }
| EQQ   { Eq  }
| AND   { And }
| OR    { Or  }
;

const:
| i = CST { Int(i) }
;



