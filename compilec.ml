open Ast
open Ast_mips
open Compile_mips
open Errors

exception Error_no_pos of string

let functions = Hashtbl.create 10

let load_print_int =
  Hashtbl.replace functions "print_int"  ( Dvoid , [] )

let convert_arith (o:binop) = match o with
| Add -> (+)
| Sub -> (-)
| Mul -> ( * )
| Div -> (/)
| Mod -> (mod)
| _ -> raise (Error_no_pos "pas une op arithmétique")

let convert_comp (o:binop) = match o with
| Leq -> (<=)
| Le -> (<)
| Geq -> (>=)
| Ge -> (>)
| Neq -> (!=)
| Eq -> (=)
| _ -> raise (Error_no_pos "pas une op de comparaison")

let convert_cond (o:binop) = match o with
| And -> (&&)
| Or -> (||)
| _ -> raise (Error_no_pos "pas and ou or")

let int_of_bool b = if b then 1 else 0
let bool_of_int i = if i=0 then false else true

let rec eval_expr hashtable_loc e =
  match e with
  | Const Int c -> Iconst(int_of_string c)
  | Val v -> (match v with
    | Var x -> (match Hashtbl.find_opt hashtable_loc x with
      | None -> raise (Error_no_pos "Undefined value")
      | Some (_,i) -> i)
  )
  | Moins e ->
    (match eval_expr hashtable_loc e with
    | Iconst c -> Iconst (-1 * c)
    | _ -> raise (Error_no_pos "Invalid expression"))
  | Not n -> (
    match eval_expr hashtable_loc n with
    | Iconst c -> if c=0 then Iconst 1 else Iconst 0
    | _ -> raise (Error_no_pos "Invalid expression")
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
  | Ecall (name,expl) -> check_call hashtable_loc name expl
  | _ -> raise (Error_no_pos " Array à faire plus tard")

  and check_call hashtable_loc name expl =
    if Hashtbl.mem functions name then (
      let args_compile =  List.map (eval_expr hashtable_loc) expl in (* on compile d'abord les arguments que prends la fonction name*)
      let expr_to_type expr = match expr with 
        | Icall( s , expr_l) -> fst (Hashtbl.find functions s )
        | _ -> Dint 
       in
      match name with 
        | "print_int" -> if List.mem Dvoid (List.map expr_to_type args_compile) then raise (Error_no_pos ("wrong arguments for print_int")) 
        else Icall(name,args_compile)
        | _ -> 
          if (List.map expr_to_type args_compile = snd (Hashtbl.find functions name) ) then Icall(name, args_compile)
          else
            raise (Error_no_pos (" wrong arguments for " ^ name))
    )
    else raise (Error_no_pos ("Undefined function "^name)) (* TODO: check the type + check if funtion exist*)

let compile_if hashtable_loc cond body =
  let e = eval_expr hashtable_loc cond in
  match e with
    | Iconst k -> if k <> 0 then body else No_op
    | Icall(name,_) -> if fst (Hashtbl.find functions name) <> Dint then raise (Error_no_pos "if condition cannot be of type <void>") else Iif(e, body)
    | _ -> Iif(e, body)

let compile_if_else hashtable_loc cond body_if body_else =
  let e = eval_expr hashtable_loc cond in
  match e with
    | Iconst k -> if k <> 0 then body_if else body_else
    | Icall(name,_) -> if fst (Hashtbl.find functions name) <> Dint then raise (Error_no_pos "if condition cannot be of type <void>") else Iifelse(e, body_if, body_else)
    | _ -> Iifelse(e, body_if, body_else)

let rec compile_stmt hashtable_loc (stmt,pos) = try (
  match stmt with
  | (Sassign (l,exp)) ->
    (match l with
    | Var x -> if Hashtbl.mem hashtable_loc x then (
      let exp_eval = eval_expr hashtable_loc exp in
      let x_num = fst(Hashtbl.find hashtable_loc x) in
      Hashtbl.replace hashtable_loc x (x_num, exp_eval) ;
      Iassign ((Ilocal x_num, 4) , exp_eval)) else raise (Error_no_pos ("Undefined variable "^x)) )
  | (Sval e) -> Ival (eval_expr hashtable_loc e)
  | (Sreturn e) ->  Ireturn (eval_expr hashtable_loc e)
  | (Sblock b) -> Iblock (List.map (compile_stmt hashtable_loc) b)
  | Sif (cond, stmt) -> compile_if hashtable_loc cond (compile_stmt hashtable_loc stmt)
  | Sifelse (cond, e_if, e_else) -> No_op
  | (Sdeclarevar (typ,Var x)) -> if Hashtbl.mem hashtable_loc x then raise (Error_no_pos ("error: redeclaration of " ^ x ^ " with no linkage"))
    else (
      if typ = Dint then (Hashtbl.add hashtable_loc x (Hashtbl.length hashtable_loc, IUndef) ; No_op )
      else raise (Error_no_pos (x ^ " cannot be of type <void>"))
      )(* Assignement de variable à définir*)
  ) with
    | Error_no_pos s -> raise (Error (s,pos))
  (* | _ -> raise (Error_no_pos "à faire") *)


(* Compile le programme p et enregistre le code dans le fichier ofile *)
let compile_program p ofile =
  load_print_int;
  let aux x =
    if Hashtbl.mem functions x.name then raise (Error_no_pos ("redeclaration of " ^ x.name ^ " with no linkage"))
    else  Hashtbl.add functions x.name (x.ret_type, List.map fst x.args);
    match x.body with
    | None -> (x.name, 0 ,List.length x.args,Iassign((Iglobal x.name, 4), IUndef))(* à modifier car comment gérer la déclaration de variable, convention à définir *)
    | Some x_body -> let hashtable_loc = Hashtbl.create 5 in
    List.iteri (fun i (t,name) -> Hashtbl.replace hashtable_loc name (i, Ileft (Ilocal i,4))) x.args; (*declare les arguments*)
    let body = compile_stmt hashtable_loc x_body in
      (x.name, Hashtbl.length hashtable_loc, List.length x.args, body) in
  let code = List.map aux p in
  to_mips (code,[]) ofile

  (* let p = to_mips { text = code; data = [] } in
  Mips.print_program p ofile *)
