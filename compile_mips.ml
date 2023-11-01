(* Production de code pour notre langage *)

open Mips
open Ast_mips
open Errors

let label_cnt = ref 1

(* Les fonctions de manipulation de la pile pour rendre les instructions plus modulaires *)
let push_var reg x nb = [ Arithi (Add, SP, SP, -4); Sw (reg, Areg (0, SP)) ]

(* Empiler une variable temporaire *)
let push_tmp = [ Arithi (Add, SP, SP, -4); Sw (V 0, Areg (0, SP)) ]

(* Dépiler *)
let pop_tmp =
  (* sp_aux := !sp_aux - 1; *)
  [ Lw (V 0, Areg (0, SP)); Arithi (Add, SP, SP, 4) ]

(*allocate local vars*)
let allocate_mem nb_vars = [ Arithi (Add, SP, SP, -4 * nb_vars) ]

(* free local vars*)
let free_mem nb_vars = [ Arithi (Add, SP, SP, 4 * nb_vars) ]

(*save fp on stack*)
let save_fp offset =
  [ Sw (FP, Areg (4 * offset, SP)); Arithi (Add, FP, SP, 4 * offset) ]

let restore_fp nb_vars = Lw (FP, Areg (0, FP)) :: free_mem (nb_vars + 1)
let save_ra offset = [ Sw (RA, Areg (4 * offset, SP)) ]

(* restore ra, restore fp, free local vars, jmp ra*)
let end_of_fun is_main nb_var =
  if is_main then
    (Lw (FP, Areg (0, FP)) :: free_mem (nb_var + 1)) @ [ End_of_program ]
  else
    (Lw (RA, Areg (4, FP)) :: Lw (FP, Areg (0, FP)) :: free_mem (nb_var + 2))
    @ [ Jr RA ]

(*puts label, save fp and ra, move sp position in fp, allocate memory for local vars *)
let start_of_fun name nb_vars =
  (Label name :: allocate_mem (nb_vars + 2))
  @ save_ra (nb_vars + 1)
  @ save_fp nb_vars

(*retrive var postiion in stack*)
let get_var_addr size offset = Areg (-size * (offset + 1), FP)

let arith_of_binop = function
  | Ast.Add -> Mips.Add
  | Ast.Sub -> Mips.Sub
  | Ast.Div -> Mips.Div
  | Ast.Mul -> Mips.Mul
  | _ -> failwith "non equivalent"

let rec compile_i_ast is_main nb_var = function
  | Iif (e, b_if) -> compile_i_if is_main nb_var e b_if
  | Iifelse (e, b_if, b_else) -> compile_i_if_else is_main nb_var e b_if b_else
  | Iblock a -> List.map (compile_i_ast is_main nb_var) a |> List.concat
  | Ireturn e -> compile_i_expr e @ end_of_fun is_main nb_var
  | Iassign (l, e) -> compile_i_assign (l, e)
  | Ival e -> compile_i_expr e
  | No_op -> []

and compile_i_left_value (ipos, size) =
  match ipos with
  | Ilocal offset -> [ Lw (V 0, get_var_addr size offset) ]
  | Iglobal label -> [ Lw (V 0, Alab label) ]
  | Ideref p -> []

and compile_i_const_value k = [ Li (V 0, k) ]
and compile_i_moins v = compile_i_expr v @ [ Arithi (Mul, V 0, V 0, -1) ]
and compile_i_not v = compile_i_expr v @ [ Sltu (V 0, V 0, 1) ]

and compile_i_op (op : Ast.binop) =
  match op with
  | Add | Sub | Mul | Div -> [ Arith (arith_of_binop op, V 0, V 0, V 1) ]
  | Mod -> [ Arith_div (V 0, V 1); Mfhi (V 0) ]
  | Leq -> [ Slt (V 0, V 1, V 0); Xori (V 0, V 0, 1) ] (* a <= b = ! b < a*)
  | Le -> [ Slt (V 0, V 0, V 1) ]
  | Geq -> [ Slt (V 0, V 0, V 1); Xori (V 0, V 0, 1) ]
  | Ge -> [ Slt (V 0, V 1, V 0) ]
  | Neq -> [ Xor (V 0, V 0, V 1); Slt (V 0, ZERO, V 0) ]
  | Eq -> [ Xor (V 0, V 0, V 1); Sltu (V 0, V 0, 1) ]
  | And -> [ And (V 0, V 0, V 1) ]
  | Or -> [ Or (V 0, V 0, V 1) ]

and compile_i_binop op a = function
  | Iconst k -> compile_i_expr a @ [ Li (V 1, k) ] @ compile_i_op op
  | _ as b ->
      compile_i_expr a @ push_tmp @ compile_i_expr b
      @ [ Move (V 1, V 0); Lw (V 0, Areg (0, SP)) ]
      @ compile_i_op op
      @ [ Arithi (Add, SP, SP, 4) ]

and compile_i_assign (l, e) =
  let tmp = compile_i_expr e and ipos, size = l in
  match ipos with
  | Ilocal offset -> tmp @ [ Sw (V 0, get_var_addr size offset) ]
  | Iglobal label -> tmp @ [ Sw (V 0, Alab label) ]
  | Ideref p -> tmp @ []

and compile_i_args =
  (*compile les ai*)
  let rec loop_ai i = function
    | e :: [] -> compile_i_expr e @ [ Move (A i, V 0) ]
    | e :: l' ->
        (*stocke temporairement la valeur le temps de caculer le reste*)
        compile_i_expr e @ push_tmp
        @ loop_ai (i + 1) l'
        (*recupere le resultat temporaire qui sera dans a_i*)
        @ pop_tmp
        @ [ Move (A i, V 0) ]
    | [] -> []
  and loop_spi res = function
    | [] -> res
    | e :: l' -> loop_spi (res @ compile_i_expr e @ push_tmp) l'
  in
  function
  | a0 :: a1 :: a2 :: a3 :: l' ->
      (* on fait d'abord la loop sur les arguments qui iront sur la pile puis ceux qui iront dans les a_i*)
      loop_spi [] l' @ loop_ai 0 (a0 :: a1 :: a2 :: [ a3 ])
  | args -> loop_ai 0 args

and compile_i_expr = function
  | Iconst k -> compile_i_const_value k
  | Ileft lv -> compile_i_left_value lv
  | Imoins v -> compile_i_moins v
  | Inot v -> compile_i_not v
  | Ibinop (op, a, b) -> compile_i_binop op a b
  | IUndef -> failwith "Not done"
  | Icall (label, args) -> (
      match label with
      | "print_int" -> i_print_int [] args
      | label -> compile_i_args args @ [ Jal label ])

and i_print_int res = function
  | [] -> res @ [ Li (V 0, 11); Li (A 0, 10); Syscall ]
  | e :: [] ->
      res @ compile_i_expr e
      @ [
          Move (A 0, V 0);
          Li (V 0, 1);
          Syscall;
          Li (V 0, 11);
          Li (A 0, 10);
          Syscall;
        ]
  | e :: l' ->
      i_print_int
        (res @ compile_i_expr e
        @ [
            Move (A 0, V 0);
            Li (V 0, 1);
            Syscall;
            Li (V 0, 11);
            Li (A 0, 32);
            Syscall;
          ])
        l'

and compile_i_if is_main nb_var cond b_if =
  label_cnt := !label_cnt + 1;
  compile_i_expr cond
  @ [ Beq (V 0, ZERO, jlabel !label_cnt) ]
  @ compile_i_ast is_main nb_var b_if
  @ [ Label (jlabel !label_cnt) ]

and compile_i_if_else is_main nb_var cond b_if b_else =
  label_cnt := !label_cnt + 2;
  compile_i_expr cond
  @ [ Beq (V 0, ZERO, jlabel (!label_cnt - 1)) ]
  @ compile_i_ast is_main nb_var b_if
  @ [ J (jlabel !label_cnt) ]
  @ [ Label (jlabel (!label_cnt - 1)) ]
  @ compile_i_ast is_main nb_var b_else
  @ [ Label (jlabel !label_cnt) ]

let allocate_args nb_args =
  let rec loop_ai i stop =
    if i >= stop then []
    else [ Sw (A i, get_var_addr 4 i) ] @ loop_ai (i + 1) stop
  and loop_spi res i =
    if i >= nb_args then res
    else
      loop_spi
        (res
        @ [
            Lw (T 0, Areg (4 * (nb_args + 1 - i), FP));
            Sw (T 0, get_var_addr 4 i);
          ])
        (i + 1)
  in
  if nb_args <= 3 then loop_ai 0 nb_args else loop_ai 0 4 @ loop_spi [] 4

let compile_i_no_return is_main nb_vars ins =
  let l =
    match List.rev ins with
    | x :: l' as ins -> (
        match x with
        | J ra -> ins
        | _ -> List.rev_append (end_of_fun is_main nb_vars) ins)
    | _ as ins -> ins
  in
  List.rev l

let compile_main name nb_vars = function
  | No_op -> []
  | body ->
      let l =
        (Label name :: allocate_mem (nb_vars + 1))
        @ save_fp nb_vars
        @ compile_i_ast true nb_vars body
      in
      compile_i_no_return true nb_vars l

let compile_fun name nb_vars nb_args = function
  | No_op -> []
  | body ->
      let l =
        start_of_fun name nb_vars @ allocate_args nb_args
        @ compile_i_ast false nb_vars body
      in
      compile_i_no_return false nb_vars l

(* Compile le programme p et enregistre le code dans le fichier ofile *)
let to_mips (p, data) ofile =
  let aux (name, nb_var, nb_args, body) =
    match name with
    | "main" -> compile_main name nb_var body
    | name -> compile_fun name nb_var nb_args body
  in
  let code = List.map aux p |> List.concat in
  let p = { text = code; data = [] } in
  Mips.print_program p ofile
