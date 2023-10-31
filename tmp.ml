open Ast
open Ast_mips
open Compile_mips
open Errors

exception Error_no_pos of string

let functions = Hashtbl.create 10
let load_print_int = Hashtbl.replace functions "print_int" (Dvoid, [])

let convert_arith (o : binop) =
  match o with
  | Add -> ( + )
  | Sub -> ( - )
  | Mul -> ( * )
  | Div -> ( / )
  | Mod -> ( mod )
  | _ -> raise (Error_no_pos "pas une op arithmétique")

let convert_comp (o : binop) =
  match o with
  | Leq -> ( <= )
  | Le -> ( < )
  | Geq -> ( >= )
  | Ge -> ( > )
  | Neq -> ( != )
  | Eq -> ( = )
  | _ -> raise (Error_no_pos "pas une op de comparaison")

let convert_cond (o : binop) =
  match o with
  | And -> ( && )
  | Or -> ( || )
  | _ -> raise (Error_no_pos "pas and ou or")

let int_of_bool b = if b then 1 else 0
let bool_of_int i = if i = 0 then false else true

(* simplifie les expressions, en remplçant 4+3 par 7 mais en laissant 4 + f(3), sans calculer f(3)
   renvoie des iexprs *)
let rec eval_expr hashtable_loc e =
  match e with
  | Const (Int c) -> Iconst (int_of_string c)
  | Val v -> (
      match v with
      | Var x -> (
          match Hashtbl.find_opt hashtable_loc x with
          | None -> raise (Error_no_pos "Undefined value")
          | Some (_k, i) -> i))
  | Moins e -> (
      match eval_expr hashtable_loc e with
      | Iconst c -> Iconst (-1 * c)
      | _ -> raise (Error_no_pos "Invalid expression"))
  | Not n -> (
      match eval_expr hashtable_loc n with
      | Iconst c -> if c = 0 then Iconst 1 else Iconst 0
      | _ -> raise (Error_no_pos "Invalid expression"))
  | Op (op, e1, e2) -> (
      match op with
      | Add | Sub | Mul | Div | Mod -> (
          match (eval_expr hashtable_loc e1, eval_expr hashtable_loc e2) with
          | Iconst i1, Iconst i2 -> Iconst (convert_arith op i1 i2)
          | ie1, ie2 ->
              Ibinop (op, ie1, ie2)
              (* on ne fait rien car ce ne sont pas directement deux constantes *)
          )
      | Leq | Le | Geq | Ge | Neq | Eq -> (
          match (eval_expr hashtable_loc e1, eval_expr hashtable_loc e2) with
          | Iconst i1, Iconst i2 -> Iconst (int_of_bool (convert_comp op i1 i2))
          | ie1, ie2 -> Ibinop (op, ie1, ie2) (* idem *))
      | And | Or -> (
          match (eval_expr hashtable_loc e1, eval_expr hashtable_loc e2) with
          | Iconst i1, Iconst i2 ->
              Iconst
                (int_of_bool
                   (convert_cond op (bool_of_int i1) (bool_of_int i2)))
          | ie1, ie2 -> Ibinop (op, ie1, ie2) (* idem *)))
  | Ecall (name, expl) -> check_call hashtable_loc name expl
  | _ -> raise (Error_no_pos " Array à faire plus tard")

(* vérifie que la fonction est définie et que les arguments expl correspondent à ceux attendu par la fonction*)
and check_call hashtable_loc name expl =
  if Hashtbl.mem functions name then
    (* la fonction name est déjà définie *)
    let args_compile = List.map (eval_expr hashtable_loc) expl in
    (* on compile d'abord les arguments que prend name*)
    let expr_to_type expr =
      match expr with
      (* la fonction expr_to_type permettra de comparer les types des éléments de expl avec les types des arguments attendus par name *)
      | Icall (s, expr_l) -> fst (Hashtbl.find functions s)
      | _ -> Dint
      (* une expression est forcément un int si ce n'est pas un ecall *)
    in
    match name with
    | "print_int" ->
        if List.mem Dvoid (List.map expr_to_type args_compile) then
          raise (Error_no_pos "wrong arguments for print_int")
        else Icall (name, args_compile)
          (* la fonction print_int prend un nombre infini d'argument, il suffit donc de regarder si un argument n'est pas un int*)
    | _ ->
        (* sinon il faut aussi que le nombre d'argument expl soit égale au nombre d'argument attendu par name *)
        if
          List.map expr_to_type args_compile = snd (Hashtbl.find functions name)
          (* compare taille et type *)
        then Icall (name, args_compile)
        else raise (Error_no_pos (" wrong arguments for " ^ name))
  else raise (Error_no_pos ("Undefined function " ^ name))

let compile_if hashtable_loc cond body =
  match eval_expr hashtable_loc cond with
  | Iconst k -> if k <> 0 then body else No_op
  | Icall (name, _) as e ->
      if fst (Hashtbl.find functions name) <> Dint then
        raise (Error_no_pos "if condition cannot be of type <void>")
      else Iif (e, body)
  | _ as e -> Iif (e, body)

let compile_if_else hashtable_loc cond body_if body_else =
  match eval_expr hashtable_loc cond with
  | Iconst k -> if k <> 0 then body_if else body_else
  | Icall (name, _) as e ->
      if fst (Hashtbl.find functions name) <> Dint then
        raise (Error_no_pos "if condition cannot be of type <void>")
      else Iifelse (e, body_if, body_else)
  | _ as e -> Iifelse (e, body_if, body_else)

let compile_assign hashtable_loc exp =
  let aux x x_num = function
    | Iconst k as exp_eval ->
        Hashtbl.replace hashtable_loc x (x_num, exp_eval);
        Iassign ((Ilocal x_num, 4), exp_eval)
    | _ as exp_eval ->
        Hashtbl.replace hashtable_loc x (x_num, Ileft (Ilocal x_num, 4));
        Iassign ((Ilocal x_num, 4), exp_eval)
  in
  function
  | Var x ->
      if Hashtbl.mem hashtable_loc x then
        let exp_eval = eval_expr hashtable_loc exp in
        let x_num = fst (Hashtbl.find hashtable_loc x) in
        aux x x_num exp_eval
      else raise (Error_no_pos ("Undefined variable " ^ x))

let compile_declare hashtable_loc typ x =
  if Hashtbl.mem hashtable_loc x then
    raise (Error_no_pos ("error: redeclaration of " ^ x ^ " with no linkage"))
  else if typ = Dint then (
    Hashtbl.add hashtable_loc x (Hashtbl.length hashtable_loc, IUndef);
    No_op)
  else raise (Error_no_pos (x ^ " cannot be of type <void>"))
(* Assignement de variable à définir*)

(* compile les statements grâce à eval_expr et renvoie des istatements *)
let rec compile_stmt hashtable_loc (stmt, pos) =
  try
    match stmt with
    | Sassign (l, exp) -> compile_assign hashtable_loc exp l
    | Sval e -> Ival (eval_expr hashtable_loc e)
    | Sreturn e -> Ireturn (eval_expr hashtable_loc e)
    | Sblock b -> Iblock (List.map (compile_stmt hashtable_loc) b)
    | Sif (cond, stmt) ->
        compile_if hashtable_loc cond (compile_stmt hashtable_loc stmt)
    | Sifelse (cond, e_if, e_else) ->
        compile_if_else hashtable_loc cond
          (compile_stmt hashtable_loc e_if)
          (compile_stmt hashtable_loc e_else)
    | Sinitvar (var_type, Var var_name, value) ->
        let _ = compile_declare hashtable_loc var_type var_name in
        compile_assign hashtable_loc value (Var var_name)
    | Sdeclarevar (typ, Var x) -> compile_declare hashtable_loc typ x
  with Error_no_pos s -> raise (Error (s, pos))
(* | _ -> raise (Error_no_pos "à faire") *)

(* Compile le programme p et enregistre le code dans le fichier ofile *)
let compile_program p ofile =
  load_print_int;
  let aux x =
    if Hashtbl.mem functions x.name then
      raise (Error_no_pos ("error: redefinition of " ^ x.name))
      (* C ne permet pas de déclarer deux fonctions avec le même nom*)
    else Hashtbl.add functions x.name (x.ret_type, List.map fst x.args);
    (* ajoute la nouvelle variable gloable/fonction à la table*)
    match x.body with
    | None ->
        (x.name, 0, List.length x.args, Iassign ((Iglobal x.name, 4), IUndef))
        (* c'est une variables globale *)
    | Some x_body ->
        let hashtable_loc = Hashtbl.create 5 in
        (* créé la table des variables locales à la fonction x.name *)
        List.iteri
          (fun i (t, name) ->
            Hashtbl.replace hashtable_loc name (i, Ileft (Ilocal i, 4)))
          x.args;
        (*declare les arguments, permettant de gérer l'utilisation des arguments comme une variable locale à la fonction*)
        let body = compile_stmt hashtable_loc x_body in
        (x.name, Hashtbl.length hashtable_loc, List.length x.args, body)
  in
  let code = List.map aux p in
  to_mips (code, []) ofile
