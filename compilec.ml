open Ast
open Mips
open Ast_mips

module StrMap = Map.Make (String)

exception Error of string

let make_ast () = 5

let to_mips program = program

let convert_arith (o:binop) = match o with
| Add -> (+)
| Sub -> (-)
| Mul -> ( * )
| Div -> (/)
| Mod -> (mod)
| _ -> raise (Error "pas une op arithmétique")

let convert_comp o = match o with
| Leq -> (<=)
| Le -> (<)
| Geq -> (>=)
| Ge -> (>)
| Neq -> (!=)
| Eq -> (=)
| _ -> raise (Error "pas une op de comparaison")

let convert_cond o = match o with
| And -> (&&)
| Or -> (||)
| _ -> raise (Error "pas and ou or")

let int_of_bool b = if b then 1 else 0
let bool_of_int i = if i=0 then false else true

let rec eval_expr hashtab e =
  match e with
  | Const Int c -> Iconst(int_of_string c)
  | Val v -> (match v with
    | Var x -> (match Hashtbl.find_opt hashtab x with
      | None -> raise (Error "undefined value")
      | Some i -> Iconst i)
    | Tab (a,b) -> failwith("on verra après")
  )
  | Moins e ->
    (match eval_expr hashtab e with
    | Iconst c -> Iconst (-1 * c)
    | _ -> raise (Error "expression invalide"))
  | Not n -> (
    match eval_expr hashtab n with
    | Iconst c -> if c=0 then Iconst 1 else Iconst 0
    | _ -> raise (Error "expression invalide")
  )
  | Op (op,e1,e2) ->
    (match op with
    | Add | Sub | Mul | Div | Mod -> match (eval_expr hashtab e1, eval_expr hashtab e2) with 
      |(Iconst i1 , Iconst i2) ->  Iconst (convert_arith o i1 i2)
      |(ie1,ie2) -> Binop (op,ie1,ie2)
    | Leq | Le | Geq | Ge | Neq | Eq -> match (eval_expr hashtab e1, eval_expr hashtab e2) with 
      |(Iconst i1 , Iconst i2) ->  Iconst (int_of_bool (convert_comp o i1 i2))
      |(ie1,ie2) -> Binop (op,ie1,ie2)
    | And | Or -> match (eval_expr hashtab e1, eval_expr hashtab e2) with 
      |(Iconst i1 , Iconst i2) ->  Iconst (int_of_bool (convert_cond o (bool_of_int i1) (bool_of_int i2)))
      |(ie1,ie2) -> Binop (op,ie1,ie2)
  )
  | Ecall (name,expl) -> failwith("a faire")

let compile_stmt hashtab (stmt,pos) =
  match stmt with
  | Sassign (l,exp) ->
    match l with
    | Var x -> Hashtbl.add hashtab x (eval_expr hashtab exp)
    | Tab (a,b) -> failwith("on verra après")
  | Sval e -> eval_expr hashtab e
  | _ -> failwith("a faire")

(* Compile le programme p et enregistre le code dans le fichier ofile *)
let compile_program p ofile =
  let code = List.map compile_stmt p |> List.concat in
  let p = to_mips { text = code; data = [] } in
  Mips.print_program p ofile
