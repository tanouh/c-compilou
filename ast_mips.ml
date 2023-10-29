open Ast

type i_program = (string*int* i_AST) list * (string*int) list (* (fun_name * nb_local_var * body ) * (label * size) *)
and i_AST =
| Iif of i_expr*i_AST*i_AST
| Iblock of i_AST list
| Ireturn of i_expr
| Iassign of i_left_value*i_expr
| Ival of i_expr
| No_op
and i_left_value = i_pos * int (* position in memory and size *)
and i_pos =
| Ilocal of int (* offset to FP *)
| Iglobal of string (* label *)
| Ideref of i_expr (* for pointers *)
and i_expr =
| Iunop of i_expr
| Ibinop of binop * i_expr * i_expr
| Icall of string * i_expr list(* label *)
| Ileft of i_left_value
| Iconst of int
