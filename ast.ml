type pos = Lexing.position

type stmt = stmt_node*pos
and stmt_node =
  (* | Sfor of string*expr*stmt *)
  | Sblock of stmt list
  | Sreturn of expr
  | Sassign of left_value*expr
  | Sval of expr
  | Sdeclarevar of dtype * left_value
  (* | Sif of expr*stmt
  | Sifelse of expr*stmt*stmt
  | Swhile of expr*stmt *)
and const =
  | Int of string
  (* | Str of string
  | Bool of bool *)
  (* | Non *)
and left_value =
  | Var of string

and expr =
  | Const of const
  | Val of left_value
  | Moins of expr
  | Not of expr
  | Op of binop * expr*expr
  | Array of expr list
  | Ecall of string*expr list
and binop = Add | Sub | Mul | Div | Mod | Leq | Le | Geq | Ge | Neq | Eq | And | Or
and dtype = Dint | Dvoid

type def = { ret_type : dtype ; name : string ; body : stmt option ; args : string list ; }
and cprogram = def list

let str_op = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Mod -> "Mod"
  | Leq -> "<="
  | Le -> "<"
  | Geq -> ">="
  | Ge -> ">"
  | Neq -> "!="
  | Eq -> "=="
  | And -> "&&"
  | Or -> "||"

