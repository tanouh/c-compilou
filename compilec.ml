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
      | Some i -> i)
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
    | Add | Sub | Mul | Div | Mod -> (match (eval_expr hashtab e1, eval_expr hashtab e2) with 
      |(Iconst i1 , Iconst i2) ->  Iconst (convert_arith op i1 i2)
      |(ie1,ie2) -> Ibinop (op,ie1,ie2))
    | Leq | Le | Geq | Ge | Neq | Eq -> (match (eval_expr hashtab e1, eval_expr hashtab e2) with 
      |(Iconst i1 , Iconst i2) ->  Iconst (int_of_bool (convert_comp op i1 i2))
      |(ie1,ie2) -> Ibinop (op,ie1,ie2))
    | And | Or -> (match (eval_expr hashtab e1, eval_expr hashtab e2) with 
      |(Iconst i1 , Iconst i2) ->  Iconst (int_of_bool (convert_cond op (bool_of_int i1) (bool_of_int i2)))
      |(ie1,ie2) -> Ibinop (op,ie1,ie2))
  )
  | Ecall (name,expl) -> Icall(Iglobal name, name, 0, List.map (eval_expr hashtab) expl) (* 0 à modifier *)

let compile_stmt (stmt,_pos) =
  match stmt with
  | Sassign (l,exp) ->
    (match l with
    | Var x -> let expeval = eval_expr exp in Hashtbl.add hashtab x expeval ; Iassign ("mdr",expeval)
    | Tab (a,b) -> failwith("on verra après"))
  | Sval e -> let expr = eval_expr hashtab e in Ival expr
  | Sreturn r -> let expr = eval_expr hashtab e in Ireturn expr
  | Sblock b -> List.iter compile_stmt b ; Iblock b
  | Sdeclarevar (typ,var) -> (match var with
    | Var x -> Hashtbl.add hashtab x (Iconst 0) ; Iassign (var,Iconst 0))
  | _ -> failwith("a faire")

let hashtable_loc = Hashtbl.create 20

(* Compile le programme p et enregistre le code dans le fichier ofile *)
let compile_program p ofile =
  let code = List.map (fun x -> compile_stmt hashtable_loc (x,0)) p |> List.concat in
  let p = to_mips { text = code; data = [] } in
  Mips.print_program p ofile
