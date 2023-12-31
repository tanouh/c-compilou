
/* Analyseur syntaxique pour Arith */

%{
  open Ast
%}

%token <string> CST
%token <string> IDENT
%token RETURN IF ELSE
%token EOF
%token LP RP LBRACE RBRACE
%token PLUS MINUS TIMES DIV MOD
%token COMMA SEMICOLON
%token AND OR
%token EQ EQQ
%token LEQ GEQ LE GE NEQ
%token NOT
%token INT VOID

/* Definitions des priorites et associativites des tokens */

%left OR AND
%nonassoc EQQ NEQ
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc uminus

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

arg:
arg_t = dtype ; name = IDENT; {(arg_t, name)}

def :
| ret = dtype ; s = IDENT; LP ; args = separated_list(COMMA, arg) ; RP ; SEMICOLON {{ret_type = ret ; name = s ; args = args ; body = Sno_op,$startpos }}
| ret = dtype ; s = IDENT ; LP ; args = separated_list(COMMA, arg) ; RP ; LBRACE ; b = suite ; RBRACE
{{ret_type = ret ; name = s ; args = args ; body = b }}

left_value:
| s = IDENT { Var(s) }

simple_stmt:
  | RETURN ; e = expr { Sreturn(e), $startpos }
  | l_type = dtype; l = left_value {Sdeclarevar(l_type, l), $startpos}
  | l = left_value ; EQ ; e = expr { Sassign(l,e), $startpos }
  | l_type = dtype; l = left_value ; EQ ; e = expr ; { Sinitvar (l_type, l ,e), $startpos}
  | e = expr { Sval(e), $startpos }
;

stmt:
| s = simple_stmt ; SEMICOLON  { s }
// | FOR ; s = IDENT ; IN ; e = expr ; COLON ; b = suite  {Sfor(s,e,b), $startpos}
| IF ; LP ; cond = expr ; RP ; b = simple_stmt; SEMICOLON  {Sif(cond, b), $startpos}
| IF ; LP; cond = expr ; RP ; b = simple_stmt; SEMICOLON ; ELSE ;  e = simple_stmt; SEMICOLON;  {Sifelse(cond,b,e), $startpos}
| IF ; LP ; cond = expr ; RP ; LBRACE ; b = suite; RBRACE {Sif(cond, b), $startpos}
| IF ; LP; cond = expr ; RP ; LBRACE; b = suite; RBRACE ; ELSE ; LBRACE  e = suite; RBRACE;  {Sifelse(cond,b,e), $startpos}
| IF ; LP; cond = expr ; RP ; LBRACE; b = suite; RBRACE ; ELSE ;  e = simple_stmt; SEMICOLON;  {Sifelse(cond,b,e), $startpos}
| IF ; LP; cond = expr ; RP ; b = simple_stmt; SEMICOLON ; RBRACE ; ELSE ; LBRACE  e = suite; RBRACE;  {Sifelse(cond,b,e), $startpos}
// | WHILE ; cond = expr ; COLON; b = suite {Swhile(cond,b), $startpos}
;

expr:
| c = const                      { Const(c) }
| l = left_value                 { Val(l)}
| e1 = expr o = op e2 = expr     { Op(o,e1,e2) }
| MINUS e = expr %prec uminus    { Moins(e) }
| NOT e = expr                   { Not(e) }
| s = IDENT ; LP ; args = separated_list(COMMA,expr) ; RP { Ecall(s,args) }
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



