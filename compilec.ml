open Ast
open Ast_mips
open Compile_mips

module StrMap = Map.Make (String)

exception Error of string

let make_ast () = 5

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

let rec eval_expr hashtable_loc e =
  match e with
  | Const Int c -> Iconst(int_of_string c)
  | Val v -> (match v with
    | Var x -> (match Hashtbl.find_opt hashtable_loc x with
      | None -> raise (Error "undefined value")
      | Some i -> i)
    | Tab (a,b) -> failwith("on verra après")
  )
  | Moins e ->
    (match eval_expr hashtable_loc e with
    | Iconst c -> Iconst (-1 * c)
    | _ -> raise (Error "expression invalide"))
  | Not n -> (
    match eval_expr hashtable_loc n with
    | Iconst c -> if c=0 then Iconst 1 else Iconst 0
    | _ -> raise (Error "expression invalide")
  )
  | Op (op,e1,e2) ->
    (match op with
    | Add | Sub | Mul | Div | Mod -> (match (eval_expr hashtable_loc e1, eval_expr hashtable_loc e2) with
      |(Iconst i1 , Iconst i2) ->  Iconst (convert_arith op i1 i2)
      |(ie1,ie2) -> Ibinop (op,ie1,ie2))
    | Leq | Le | Geq | Ge | Neq | Eq -> (match (eval_expr hashtable_loc e1, eval_expr hashtable_loc e2) with
      |(Iconst i1 , Iconst i2) ->  Iconst (int_of_bool (convert_comp op i1 i2))
      |(ie1,ie2) -> Ibinop (op,ie1,ie2))
    | And | Or -> (match (eval_expr hashtable_loc e1, eval_expr hashtable_loc e2) with
      |(Iconst i1 , Iconst i2) ->  Iconst (int_of_bool (convert_cond op (bool_of_int i1) (bool_of_int i2)))
      |(ie1,ie2) -> Ibinop (op,ie1,ie2))
  )
  | Ecall (name,expl) -> Icall(name, List.map (eval_expr hashtable_loc) expl) (* 0 à modifier (retire pour l'instant)*)
  | _ -> raise (Error " Array à faire plus tard")

let rec compile_stmt hashtable_loc (stmt,_pos) =
  match stmt with
  | (Sassign (l,exp), pos) ->
    (match l with
    | Var x -> if Hashtbl.mem hashtable_loc x then (let exp_eval = eval_expr hashtable_loc exp in Hashtbl.add hashtable_loc x exp_eval ;
    Iassign ((Ilocal 0,4) ,exp_eval)) else raise (Error "variable indéfinie")  (* Assignement de variable à définir*)
    | Tab (a,b) -> failwith("on verra après"))
  | (Sval e , pos) -> Ival (eval_expr hashtable_loc e)
  | (Sreturn e , pos) ->  Ireturn (eval_expr hashtable_loc e)
  | (Sblock b , pos) -> Iblock (List.map (compile_stmt hashtable_loc) (List.map (fun x -> (x,0)) b))
  | (Sdeclarevar (typ,var) , pos) -> (match var with
    | Var x -> if typ = Dint then (Hashtbl.add hashtable_loc x (Iconst 0) ; No_op )
    else raise (Error "une variable ne peut pas être de type void") (* Assignement de variable à définir*)
    | Tab (a,b) -> raise (Error "à faire"))
  (* | _ -> raise (Error "à faire") *)

let hashtable_loc = Hashtbl.create 20

(* Compile le programme p et enregistre le code dans le fichier ofile *)
let compile_program p ofile =

  let aux x = match x.body with
  | None -> (x.name, 0 ,Iassign((Iglobal x.name, 4),Iconst 0))(* à modifier car comment gérer la déclaration de variable, convention à définir *)
  | Some x_body -> let hashtable_loc = Hashtbl.create 5 in let body = compile_stmt hashtable_loc (x_body,0) in
  (x.name, Hashtbl.length hashtable_loc ,body) in
  let code = List.map aux p in
  to_mips (code,[]) ofile

  (* let p = to_mips { text = code; data = [] } in
  Mips.print_program p ofile *)
