type pos = Lexing.position

type stmt = stmt_node * pos

and stmt_node =
  | Sblock of stmt list
  | Sreturn of expr
  | Sassign of left_value * expr
  | Sval of expr
  | Sdeclarevar of dtype * left_value
  | Sif of expr * stmt
  | Sinitvar of dtype * left_value * expr
  | Sifelse of expr * stmt * stmt
  | Sno_op

and const = Int of string
and left_value = Var of string

and expr =
  | Const of const
  | Val of left_value
  | Moins of expr
  | Not of expr
  | Op of binop * expr * expr
  | Array of expr list
  | Ecall of string * expr list

and binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Leq
  | Le
  | Geq
  | Ge
  | Neq
  | Eq
  | And
  | Or

and dtype = Dint | Dvoid
and farg = dtype * string

type def = { ret_type : dtype; name : string; body : stmt; args : farg list }
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
