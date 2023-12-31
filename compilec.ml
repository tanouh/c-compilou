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
  | Val (Var x) -> (
      match Hashtbl.find_opt hashtable_loc x with
      | None -> raise (Error_no_pos "Undefined value")
      | Some (_k, i) -> i)
  | Ecall (name, expl) -> (
      match Hashtbl.find_opt functions name with
      | None -> raise (Error_no_pos ("undefined reference to '" ^ name ^ "'"))
      (* Aucune opération n'est permise sur des void, on raise donc un Warning donc le cas où une fonction retourne un void.
         Cela permet de ratrapper le warning lors d'une opération quelconque sas avoir à compiler tout
         check_call vérifie le bon fonctionnement des appels de fonction entre arguments attendus et arguments reçus*)
      | Some (Dvoid, args_l) ->
          raise
            (Warning_ret_void
               (check_call hashtable_loc name (Dvoid, args_l) expl))
      | Some (Dint, args_l) -> check_call hashtable_loc name (Dint, args_l) expl
      )
  | e -> (
      (* on ratrappe le Warning_ret_void d'une fonction renvoyant un void *)
      try
        match e with
        | Moins e -> Imoins (eval_expr hashtable_loc e)
        | Not n -> Inot (eval_expr hashtable_loc n)
        | Op (op, e1, e2) -> (
            match op with
            | Add | Sub | Mul | Div | Mod -> (
                match
                  (eval_expr hashtable_loc e1, eval_expr hashtable_loc e2)
                with
                | Iconst i1, Iconst i2 -> Iconst (convert_arith op i1 i2)
                (* on ne fait rien car ce ne sont pas directement deux constantes *)
                | ie1, ie2 -> Ibinop (op, ie1, ie2))
            | Leq | Le | Geq | Ge | Neq | Eq -> (
                match
                  (eval_expr hashtable_loc e1, eval_expr hashtable_loc e2)
                with
                | Iconst i1, Iconst i2 ->
                    Iconst (int_of_bool (convert_comp op i1 i2))
                (* idem *)
                | ie1, ie2 -> Ibinop (op, ie1, ie2))
            | And | Or -> (
                match
                  (eval_expr hashtable_loc e1, eval_expr hashtable_loc e2)
                with
                | Iconst i1, Iconst i2 ->
                    Iconst
                      (int_of_bool
                         (convert_cond op (bool_of_int i1) (bool_of_int i2)))
                (* idem *)
                | ie1, ie2 -> Ibinop (op, ie1, ie2)))
        | _ ->
            raise (Error_no_pos "pour enlever le warning du pattern martching")
      with Warning_ret_void i_e ->
        raise (Error_no_pos "error: void value not ignored as it ought to be"))

(* Vérifie que la fonction est définie et que les arguments expl correspondent à ceux attendu par la fonction *)
and check_call hashtable_loc name (name_ret_type, name_dtype_args) expl =
  let args_compile =
    (* si dans les arguments il y a un void, il faut verifier :
        1. que la liste des arguments donnée à name est au plus de taille 1
        2. que la liste des arguments attendue par name est au plus de taille 1 et, si c'est le cas, contient au plus un seul Dvoid *)
    try List.map (eval_expr hashtable_loc) expl
    with Warning_ret_void i_e -> (
      match name_dtype_args with
      | [] | [ Dvoid ] ->
          if List.length expl > 1 then
            raise
              (Error_no_pos
                 ("error: they are too many arguments to function '" ^ name
                ^ "'"))
          else [ i_e ]
      | _ -> raise (Error_no_pos ("wrong arguments for '" ^ name ^ "'")))
  in
  match args_compile with
  (* Cas où (1er cas) args_compile est vide ou (2ème cas) contient une i_expr de type Dvoid *)
  (* 1er cas *)
  | [] -> (
      match name_dtype_args with
      (* Cas où name attend un void en argument, aucun problème *)
      | [] | [ Dvoid ] -> Icall (name, args_compile)
      (* Sinon name attend plusieurs arguments et ces derniers sont donc tous de type int nécessairement en C *)
      | _ ->
          raise
            (Error_no_pos ("error: wrong arguments to function '" ^ name ^ "'"))
      )
  (* 2ème cas *)
  | [ Icall (other_name, other_args) ]
    when fst (Hashtbl.find functions other_name) = Dvoid -> (
      match name_dtype_args with
      (* La fonction print_int ne prend pas de void en argument
         mais puisqu'elle prend un nombre d'argument, aussi long que l'on veut,
         par convention name_dtype_args = []
         qui correspond en C à une fonction prenant en argument un void *)
      | [] when name = "print_int" ->
          raise
            (Error_no_pos ("error: wrong arguments to function '" ^ name ^ "'"))
      (* Sinon aucun soucis la fonction name attend bien un void en argument *)
      | [] | [ Dvoid ] -> Icall (name, args_compile)
      | _ ->
          raise
            (Error_no_pos ("error: wrong arguments to function '" ^ name ^ "'"))
      )
  (* Pour print_int on ne fait pas attention au nombre d'argument si ils sont tous des int *)
  | _ when name = "print_int" -> Icall (name, args_compile)
  (* Sinon, il faut vérifier qu'il y a autant d'arguments donnés à name, que d'arguments que name attend *)
  | _ ->
      if List.length expl = List.length (snd (Hashtbl.find functions name)) then
        Icall (name, args_compile)
      else if List.length expl < List.length (snd (Hashtbl.find functions name))
      then
        raise
          (Error_no_pos
             ("error: they are too few arguments to function '" ^ name ^ "'"))
      else
        raise
          (Error_no_pos
             ("error: they are too many arguments to function '" ^ name ^ "'"))

let compile_if hashtable_loc cond body =
  match eval_expr hashtable_loc cond with
  (*optimisation : si k <> 0 alors on met body directement sinon rien*)
  | Iconst k -> if k <> 0 then body else No_op
  (*verifie que le retour de la fonction dans la condition est bien un int et pas un void*)
  | Icall (name, _) as e ->
      if fst (Hashtbl.find functions name) <> Dint then
        raise (Error_no_pos "if condition cannot be of type <void>")
      else Iif (e, body)
  | _ as e -> Iif (e, body)

let compile_if_else hashtable_loc cond body_if body_else =
  match eval_expr hashtable_loc cond with
  (*optimisation : si k <> 0 alors on met body_if directement sinon body_else*)
  | Iconst k -> if k <> 0 then body_if else body_else
  | Icall (name, _) as e ->
      (*verifie que le retour de la fonction dans la condition est bien un int et pas un void*)
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

let compile_declare hashtable_loc typ x_name =
  if Hashtbl.mem hashtable_loc x_name then
    raise
      (Error_no_pos ("error: redeclaration of " ^ x_name ^ " with no linkage"))
  else if typ = Dint then (
    Hashtbl.add hashtable_loc x_name (Hashtbl.length hashtable_loc, IUndef);
    No_op)
  else
    raise
      (Error_no_pos ("error: variable or field '" ^ x_name ^ "'declared void"))

(* compile les statements grâce à eval_expr et renvoie des istatements *)
let rec compile_stmt name hashtable_loc (stmt, pos) =
  try
    match stmt with
    | Sno_op -> No_op
    | Sassign (l, exp) -> compile_assign hashtable_loc exp l
    | Sval e -> (
        try Ival (eval_expr hashtable_loc e)
        with Warning_ret_void i_e ->
          Ival i_e (*le type de retour est un void et peut poser probleme*))
    | Sreturn e -> (
        try Ireturn (eval_expr hashtable_loc e)
        with Warning_ret_void i_e ->
          raise (Error_no_pos "error: void value not ignored as it ought to be")
        )
    | Sblock b -> Iblock (List.map (compile_stmt name hashtable_loc) b)
    | Sif (cond, stmt) ->
        compile_if hashtable_loc cond (compile_stmt name hashtable_loc stmt)
    | Sifelse (cond, e_if, e_else) ->
        compile_if_else hashtable_loc cond
          (compile_stmt name hashtable_loc e_if)
          (compile_stmt name hashtable_loc e_else)
        (* initvar = declarevar + assign*)
    | Sinitvar (var_type, Var var_name, value) -> (
        let _ = compile_declare hashtable_loc var_type var_name in
        try compile_assign hashtable_loc value (Var var_name)
        with Warning_ret_void i_e ->
          raise (Error_no_pos "error: void value not ignored as it ought to be")
        )
    | Sdeclarevar (typ, Var x) ->
        if Hashtbl.mem hashtable_loc x then
          raise
            (Error_no_pos ("error: redeclaration of " ^ x ^ " with no linkage"))
        else if typ = Dint then (
          Hashtbl.add hashtable_loc x (Hashtbl.length hashtable_loc, IUndef);
          No_op)
        else raise (Error_no_pos ("error: " ^ x ^ " cannot be of type <void>"))
    (* Assignement de variable à définir*)
  with Error_no_pos s -> raise (Error (s, pos))

(* Vérifie que le corps de chaque fonction n'est défini qu'une seul fois
    vérifie que chaque fonction n'est déclarée qu'une fois on autorise pas
   int f();
   int f();
*)
let verif_declar_function x =
  match x.body with
  | Sno_op, _ -> (
      match Hashtbl.find_opt functions_corps_existe x.name with
      (* Première défintion de x.name mais sans corps pour le moment *)
      | None -> Hashtbl.add functions_corps_existe x.name false
      | _ -> raise (Error_no_pos ("error: redefinition of " ^ x.name)))
  | x_body -> (
      match Hashtbl.find_opt functions_corps_existe x.name with
      (* x.name n'a jamais été définie. On précise donc que x.name existe et qu'elle a un corps défini quelque part *)
      | None -> Hashtbl.add functions_corps_existe x.name true
      (* x.name a déjà été définie mais sans savoir si elle a un corps. On précise donc qu'elle a un corps défini quelque part *)
      | Some false -> Hashtbl.add functions_corps_existe x.name true
      | Some true -> raise (Error_no_pos ("error: redefinition of " ^ x.name)))

(* Compile le programme p et enregistre le code dans le fichier ofile *)
let compile_program p ofile =
  load_print_int;
  (* Assure la bonne définition de chaque fonction *)
  List.iter verif_declar_function p;
  let aux x =
    (* Ajoute la nouvelle fonction à la table*)
    Hashtbl.replace functions x.name (x.ret_type, List.map fst x.args);
    match x.body with
    | Sno_op, pos ->
        if Hashtbl.find functions_corps_existe x.name then (x.name, 0, 0, No_op)
        else
          (* x.name est définie mais son corps est défini nulle part dans la def list *)
          raise (Error ("error: undefined reference to `" ^ x.name ^ "'", pos))
    | x_body ->
        (* On créé la table des variables locales à la fonction x.name *)
        let hashtable_loc = Hashtbl.create 5 in
        (* declare les arguments, permettant de gérer l'utilisation des arguments comme une variable locale à la fonction*)
        List.iteri
          (fun i (t, name) ->
            Hashtbl.replace hashtable_loc name (i, Ileft (Ilocal i, 4)))
          x.args;
        let body = compile_stmt x.name hashtable_loc x_body in
        (x.name, Hashtbl.length hashtable_loc, List.length x.args, body)
  in
  let code = List.map aux p in
  to_mips (code, []) ofile
