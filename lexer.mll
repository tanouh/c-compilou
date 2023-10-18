
(* Analyseur lexical *)

{
  open Lexing
  open Parser

  exception Lexing_error of char

  let kwd_tbl = ["int", INT; "void", VOID ; "if", IF; "else", ELSE ; "return",RETURN ]
  let id_or_kwd s = try List.assoc s kwd_tbl with _ -> IDENT s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = (letter | '_') (letter | digit | '_')*
let integer = ['0'-'9']+
let space = [' ' '\t']

rule token = parse
  | '\n'    { newline lexbuf; token lexbuf }
  | space+  { token lexbuf }
  | ident as id { id_or_kwd id }
  | ';'     { SEMICOLON }
  | '=''='  { EQQ }
  | '<''='  { LEQ }
  | '>''='  { GEQ }
  | '<'     { LE }
  | '>'     { GE }
  | '!''='  { NEQ }
  | '&''&'  { AND }
  | '|''|'  { OR }
  | '!'     { NOT }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { TIMES }
  | '/'     { DIV }
  | '%'     { MOD }
  | '='     { EQ }
  | '('     { LP }
  | ')'     { RP }
  | '{'     { LBRACE}
  | '}'     { RBRACE }
  | '['     { LB }
  | ']'     { RB }
  | '&'     { PTR }
  | ','     { COMMA }
  | integer as s { CST (s) }
  | eof     { EOF }
  | _ as c  { raise (Lexing_error c) }


