type i_program = (string*i_AST) list * (string*int) list
and i_AST =
| Iif of i_expr*i_AST*i_AST | Iblock of i_AST list
| Ireturn of i_expr | Iassign of i_left_value*i_expr
| Ival of i_expr
and i_value =
| Ileft of i_left_value | Iconst of int
and i_left_value = i_pos * int (* position in memory and size *)
and i_pos =
| Ilocal of int (* offset to FP *)
| Iglobal of string (* label *)
| Ideref of i_expr (* for pointers *)
and i_expr =
| Iunop of i_value | Ibinop of binop * i_value * i_value
| Icall of i_pos * string * int (* label + offset *)
