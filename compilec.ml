open Ast
open Ast_mips
open Compile_mips
open Errors

exception Error_no_pos of string
exception Warning_ret_void of i_expr

(* ( string, bool) Hashtbl.t *)
let functions_corps_existe = Hashtbl.create 10

(* (string, (dtype, dtype list)) Hashtbl.t *)
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

(* simplifie les expressions, en remplçant 4+3 par 7 mais en laissant 4 + f(3), sans calculer f(3)
   renvoie des iexprs *)
let rec eval_expr hashtable_loc e =
  match e with
  | Const Int c -> Iconst(int_of_string c)
  | Val Var x -> ( match Hashtbl.find_opt hashtable_loc x with
    | None -> raise (Error_no_pos "Undefined value")
    | Some (_k,i) -> i
    )
  | Ecall (name,expl) -> ( match Hashtbl.find_opt functions name with
    | None -> raise (Error_no_pos ("undefined reference to '" ^ name ^"'"))
    | Some (Dvoid, args_l) -> raise (Warning_ret_void (check_call hashtable_loc name (Dvoid, args_l) expl))
    | Some (Dint, args_l) -> check_call hashtable_loc name (Dint, args_l) expl )
  | e -> try ( match e with
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
        (* on ne fait rien car ce ne sont pas directement deux constantes *)
        |(ie1,ie2) -> Ibinop (op,ie1,ie2))
      | Leq | Le | Geq | Ge | Neq | Eq -> (match (eval_expr hashtable_loc e1, eval_expr hashtable_loc e2) with
        |(Iconst i1 , Iconst i2) ->  Iconst (int_of_bool (convert_comp op i1 i2))
        (* idem *)
        |(ie1,ie2) -> Ibinop (op,ie1,ie2))
      | And | Or -> (match (eval_expr hashtable_loc e1, eval_expr hashtable_loc e2) with
        |(Iconst i1 , Iconst i2) ->  Iconst (int_of_bool (convert_cond op (bool_of_int i1) (bool_of_int i2)))
        (* idem *)
        |(ie1,ie2) -> Ibinop (op,ie1,ie2))
    )
    | _ -> raise (Error_no_pos "pour enlever le warning du pattern martching")
    ) with
    | Warning_ret_void i_e-> raise (Error_no_pos ("error: void value not ignored as it ought to be"))

(* vérifie que la fonction est définie et que les arguments expl correspondent à ceux attendu par la fonction*)
and check_call hashtable_loc name (name_ret_type, name_dtype_args) expl =
  let args_compile = (try List.map (eval_expr hashtable_loc) expl with
    | Warning_ret_void i_e -> match name_dtype_args with
      | [] | [Dvoid] -> if List.length expl > 1 then raise (Error_no_pos("error: they are too many arguments to function '"^name^"'"))
      else [i_e]
      | _ -> raise (Error_no_pos ("wrong arguments for '" ^ name ^"'"))
  ) in
  match name with
    | "print_int" -> Icall("print_int", args_compile)
    | _ -> match args_compile with
      | [] -> ( match name_dtype_args with
          |[] | [Dvoid] -> Icall(name, args_compile)
          | _ -> raise (Error_no_pos("error: wrong arguments to function '"^name^"'" ))
        )
      | [Icall(other_name, other_args)] when fst (Hashtbl.find functions other_name) = Dvoid -> ( match name_dtype_args with
          |[] | [Dvoid] -> Icall(name, args_compile)
          | _ -> raise (Error_no_pos("error: wrong arguments to function '"^name^"'" ))
        )
      | _ -> if List.length expl = List.length (snd (Hashtbl.find functions name)) then Icall(name,args_compile)
        else if List.length expl < List.length (snd (Hashtbl.find functions name)) then raise (Error_no_pos ( "error: they are too few arguments to function '"^name^"'" ))
        else raise (Error_no_pos ( "error: they are too many arguments to function '"^name^"'" ))

let compile_if hashtable_loc cond body =
  match eval_expr hashtable_loc cond with
    | Iconst k -> if k <> 0 then body else No_op
    | Icall(name,_) as e-> if fst (Hashtbl.find functions name) <> Dint then raise (Error_no_pos "if condition cannot be of type <void>") else Iif(e, body)
    | _ as e-> Iif(e, body)

let compile_if_else hashtable_loc cond body_if body_else =
  match eval_expr hashtable_loc cond with
    | Iconst k -> if k <> 0 then body_if else body_else
    | Icall(name,_) as e-> if fst (Hashtbl.find functions name) <> Dint then raise (Error_no_pos "if condition cannot be of type <void>") else Iifelse(e, body_if, body_else)
    | _ as e -> Iifelse(e, body_if, body_else)

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

let compile_declare hashtable_loc typ x_name =
    if Hashtbl.mem hashtable_loc x_name then
      raise (Error_no_pos ("error: redeclaration of " ^ x_name ^ " with no linkage"))
    else if typ = Dint then (
      Hashtbl.add hashtable_loc x_name (Hashtbl.length hashtable_loc, IUndef);
      No_op)
    else raise (Error_no_pos ("error: variable or field '" ^ x_name ^ "'declared void"))

(* compile les statements grâce à eval_expr et renvoie des istatements *)
let rec compile_stmt name hashtable_loc (stmt,pos) = try (
  match stmt with
  | Sno_op -> No_op
  | Sassign (l,exp) -> compile_assign hashtable_loc exp l
  | Sval e -> (try Ival (eval_expr hashtable_loc e) with
    |Warning_ret_void i_e -> Ival i_e)
  | Sreturn e -> (try Ireturn (eval_expr hashtable_loc e) with
    | Warning_ret_void i_e -> raise (Error_no_pos ("error: void value not ignored as it ought to be")))
  | Sblock b -> Iblock (List.map (compile_stmt name hashtable_loc) b)
  | Sif (cond, stmt) -> compile_if hashtable_loc cond (compile_stmt name hashtable_loc stmt)
  | Sifelse (cond, e_if, e_else) -> compile_if_else hashtable_loc cond (compile_stmt name hashtable_loc e_if) (compile_stmt name hashtable_loc e_else)
  | Sinitvar (var_type, Var var_name, value) ->
    let _ = compile_declare hashtable_loc var_type var_name in
    (try compile_assign hashtable_loc value (Var var_name) with
      |Warning_ret_void i_e -> raise(Error_no_pos("error: void value not ignored as it ought to be")))
  | Sdeclarevar (typ,Var x) -> if Hashtbl.mem hashtable_loc x then raise (Error_no_pos ("error: redeclaration of " ^ x ^ " with no linkage"))
    else (
      if typ = Dint then (Hashtbl.add hashtable_loc x (Hashtbl.length hashtable_loc, IUndef) ; No_op )
      else raise (Error_no_pos ("error: " ^ x ^ " cannot be of type <void>"))
      )(* Assignement de variable à définir*)
  ) with
    | Error_no_pos s -> raise (Error (s,pos))

(* vérifie que le corps de chaque fonction n'est défini qu'une seul fois
   vérifie que chaque fonction n'est déclarée qu'une fois on autorise pas
  int f();
  int f();
*)
let verif_declar_function x = match x.body with
  | Sno_op,_ -> (match Hashtbl.find_opt functions_corps_existe x.name with
    | None -> Hashtbl.add functions_corps_existe x.name false; print_string (x.name^"\tno_op\n")
    | _ -> raise (Error_no_pos("error: redefinition of " ^ x.name)))
  | x_body -> match Hashtbl.find_opt functions_corps_existe x.name with
    | None -> Hashtbl.add functions_corps_existe x.name true; print_string (x.name^"\tadd body\n")
    | Some false -> Hashtbl.add functions_corps_existe x.name true; print_string (x.name^"\tadd body\n")
    | Some true -> raise (Error_no_pos("error: redefinition of " ^ x.name))

(* Compile le programme p et enregistre le code dans le fichier ofile *)
let compile_program p ofile =
  load_print_int;
  (* assure la bonne définition de chaque fonction *)
  List.iter verif_declar_function p ;
  let aux x = (* match Hashtbl.find_opt functions_corps_existe x.name with
    | Some false -> Hashtbl.add functions x.name (x.ret_type, List.map fst x.args);
    | Some true -> raise (Error_no_pos ("error: redefinition of " ^ x.name)) (* C ne permet pas de déclarer deux fonctions avec le même nom*)
    else  *) (* ajoute la nouvelle variable gloable/fonction à la table*)
    Hashtbl.add functions x.name (x.ret_type, List.map fst x.args);
    match x.body with
    | Sno_op ,pos -> if Hashtbl.find functions_corps_existe x.name then Hashtbl.add functions x.name (x.ret_type, List.map fst x.args)
    else raise (Error("error: undefined reference to `" ^ x.name ^ "'", pos)); (x.name, 0, 0, No_op) (* c'est une variables globale *)
    | x_body -> Hashtbl.replace functions x.name (x.ret_type, List.map fst x.args) ;let hashtable_loc = Hashtbl.create 5 in (* créé la table des variables locales à la fonction x.name *)
    List.iteri (fun i (t,name) -> Hashtbl.replace hashtable_loc name (i, Ileft (Ilocal i,4))) x.args; (*declare les arguments, permettant de gérer l'utilisation des arguments comme une variable locale à la fonction*)
    let body = compile_stmt x.name hashtable_loc x_body in
      (x.name, Hashtbl.length hashtable_loc, List.length x.args, body) in
  let code = List.map aux p in
  to_mips (code,[]) ofile

