
/* Analyseur syntaxique pour Arith */

%{
  open Ast
%}

%token <int> CST
%token <string> IDENT
%token TYPE RETURN IF ELSE 
%token EOF 
%token LP RP LBRACE RBRACE LB RB 
%token PLUS MINUS TIMES DIV MOD
%token COMMA SEMICOLON
%token AND OR 
%token EQ EQQ
%token LEQ GEQ LE GE NEQ
%token NOT PTR  

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
%type <Ast.program> prog

%%

prog:
| p = list(stmt) EOF { p }
;
def :
| ret = TYPE ; s = IDENT ; LP ; args = separated_list(COMMA,expr) ; RP ; SEMICOLON {Sdeclare(ret, s, args)}
| ret = TYPE ; s = IDENT ; LP ; args = separated_list(COMMA,expr) ; RP ; LBRACE ; b = suite ; RBRACE { Simplement(ret, s, args, b)}

simple_stmt:
  | RETURN ; e = expr ; SEMICOLON { Sreturn(e), $startpos } 
  | l = left_value ; EQ ; e = expr ; SEMICOLON { Sassign(l,e), $startpos }
  | e = expr ; SEMICOLON { Sval(e), $startpos }
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
| PTR e = INDENT                 { Pointer(e) }
| TIMES e = expr %prec uminus    {  Star(e) }
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



