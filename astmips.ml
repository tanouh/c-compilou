type Iprogram = (string*iAST) list * (string*int) list
and iAST =
| Iif of expr*iAST*iAST | Iblock of iAST list
| Ireturn of expr | Iassign of left_value*expr
| Ival of expr
and value =
| Ileft of left_value | Iconst of int
and left_value = pos * int (* position in memory and size *)
and pos =
| Ilocal of int (* offset to FP *)
| Iglobal of string (* label *)
| Ideref of expr (* for pointers *)
and expr =
| Iunop of value | Ibinop of binop * value * value
| Icall of pos * string * int (* label + offset 