open Ast
open Ast_mips
open Compile_mips
open Errors

let functions = Hashtbl.create 10

let load_print_int =
  Hashtbl.replace functions "print_int" { ret_type = Dvoid ; name = "print_int" ; body = None ; args = [] }

let convert_arith (o:binop) = match o with
| Add -> (+)
| Sub -> (-)
| Mul -> ( * )
| Div -> (/)
| Mod -> (mod)
| _ -> raise (Error "pas une op arithmétique")

let convert_comp (o:binop) = match o with
| Leq -> (<=)
| Le -> (<)
| Geq -> (>=)
| Ge -> (>)
| Neq -> (!=)
| Eq -> (=)
| _ -> raise (Error "pas une op de comparaison")

let convert_cond (o:binop) = match o with
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
      | None -> raise (Error "Undefined value")
      | Some i -> i)
  )
  | Moins e ->
    (match eval_expr hashtable_loc e with
    | Iconst c -> Iconst (-1 * c)
    | _ -> raise (Error "Invalid expression"))
  | Not n -> (
    match eval_expr hashtable_loc n with
    | Iconst c -> if c=0 then Iconst 1 else Iconst 0
    | _ -> raise (Error "Invalid expression")
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
  | _ -> raise (Error " Array à faire plus tard")

  and check_call hashtable_loc name expl =
    if Hashtbl.mem functions name then Icall(name, List.map (eval_expr hashtable_loc) expl)
    else raise (Error ("Undefined function "^name))(* TODO: check the type + check if funtion exist*)

let rec compile_stmt hashtable_loc (stmt,_pos) =
  match stmt with
  | (Sassign (l,exp), pos) ->
    (match l with
    | Var x -> if Hashtbl.mem hashtable_loc x then (let exp_eval = eval_expr hashtable_loc exp in Hashtbl.replace hashtable_loc x exp_eval ;
    Iassign ((Ilocal 0,4) ,exp_eval)) else raise (Error ("Undefined variable "^x))  (* Assignement de variable à définir*))
  | (Sval e , pos) -> Ival (eval_expr hashtable_loc e)
  | (Sreturn e , pos) ->  Ireturn (eval_expr hashtable_loc e)
  | (Sblock b , pos) -> Iblock (List.map (compile_stmt hashtable_loc) (List.map (fun x -> (x,0)) b))
  | (Sdeclarevar (typ,Var x) , pos) -> if typ = Dint then (Hashtbl.replace hashtable_loc x (IUndef) ; No_op )
    else raise (Error (x^" canno't be of type <void>")) (* Assignement de variable à définir*)

  (* | _ -> raise (Error "à faire") *)

(* Compile le programme p et enregistre le code dans le fichier ofile *)
let compile_program p ofile =
  load_print_int;
  let aux x =
    Hashtbl.replace functions x.name x;
    match x.body with
    | None -> (x.name, 0 ,List.length x.args,Iassign((Iglobal x.name, 4), IUndef))(* à modifier car comment gérer la déclaration de variable, convention à définir *)
    | Some x_body -> let hashtable_loc = Hashtbl.create 5 in
    List.iteri (fun i (t,name) -> Hashtbl.replace hashtable_loc name (Ileft (Ilocal i,4))) x.args; (*declare les arguments*)
    let body = compile_stmt hashtable_loc (x_body,0) in
      (x.name, Hashtbl.length hashtable_loc, List.length x.args,body) in
  let code = List.map aux p in
  to_mips (code,[]) ofile

  (* let p = to_mips { text = code; data = [] } in
  Mips.print_program p ofile *)
