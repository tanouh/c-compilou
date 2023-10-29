(* Production de code pour notre langage *)

open Mips
open Ast_mips

module StrMap = Map.Make (String)

(* Les fonctions de manipulation de la pile pour rendre les instructions plus modulaires *)
let push_var reg x nb =
  [ Arithi (Add, SP, SP, -4); Sw (reg, Areg (4, SP)) ]

(* Empiler une variable temporaire *)
let push_tmp =
  [ Arithi (Add, SP, SP, -4); Sw (V0, Areg (4, SP)) ]

(* Dépiler *)
let pop_tmp =
   (* sp_aux := !sp_aux - 1; *)
   [Arithi (Add, SP, SP, 4)]

   (*allocate local vars*)
let allocate_mem nb_vars =
  [Arithi (Add, SP, SP, -4 * nb_vars)]

(* free local vars*)
let free_mem nb_vars =
 [Arithi (Add, SP, SP, 4 * nb_vars)]

 (*save fp on stack*)
let save_fp = [Arithi (Add, SP, SP, -4); (Sw (FP, Areg(0,SP))); Move(FP, SP)]

let restore_fp nb_var = Lw (FP, Areg(4, FP))::free_mem (nb_var + 1)

(* restore ra, restore fp, free local vars, jmp ra*)
 let end_of_fun nb_var=
  Lw (RA, Areg(4, FP))::Lw (FP, Areg(8, FP))::free_mem (nb_var + 2)@[Jr RA]

  (*puts label, save fp and ra, move sp position in fp, allocate memory for local vars *)
let start_of_fun name nb_var =
  Label name::[Arithi (Add, SP, SP, -8); (Sw (FP, Areg(4,SP)));Sw (RA, Areg(0, SP));Move(FP, SP)]@allocate_mem(nb_var)

(*retrive var postiion in stack*)
let get_var_addr size offset =
  Areg(-size * (offset), FP)

let i_print_int =
  [ Li (V0, 1); Syscall; Li (V0, 11); Li (A0, 10); Syscall ]


let rec compile_i_ast = function
  | Iif (e, a, b) -> []
  | Iblock a -> List.map compile_i_ast a |> List.concat
  | Ireturn e -> []
  | Iassign (l,e) -> compile_i_assign (l,e)
  | Ival e -> compile_i_expr e
  |No_op -> []
and compile_i_left_value (ipos,size) = match ipos with
  | Ilocal offset -> [Lw (V0, get_var_addr size offset)]
  | Iglobal label -> [Lw (V0, Alab label)]
  |Ideref p -> []
and compile_i_const_value k = [Li (V0, k)]
and compile_i_unop v =
  (compile_i_expr v) @ [Arithi (Mul, V0, V0, -1)]
and compile_i_op op =
  match op with
  |Add | Sub | Mul | Div -> [Arith (op, V0, A0, V0)]
  | Mod -> []
  | Leq -> []
  | Le -> []
  | Geq -> []
  | Ge -> []
  | Neq -> []
  | Eq -> []
  | And -> []
  | Or-> []
and compile_i_binop op a b =
  match b with
  | Iconst k -> (compile_i_expr a) @ [Li (A0, k)] @ compile_i_op op
  | _ ->
        let l = (compile_i_expr  a) @ push_tmp in
        l @ (compile_i_expr b)
        @ [ Lw (A0, Areg (4, SP)) ]
        @ compile_i_op op
        @ pop_tmp

and compile_i_assign (l,e) =
  let tmp = compile_i_expr e and (ipos, size) = l in
  match ipos with
    | Ilocal offset -> tmp @ [Sw (V0, get_var_addr size offset)]
    | Iglobal label -> tmp @ [Sw (V0, Alab label)]
    |Ideref p -> tmp @  []
and compile_i_expr = function
  | Iconst k -> compile_i_const_value k
  | Ileft lv ->  compile_i_left_value lv
  | Iunop v -> compile_i_unop v
  | Ibinop (op, a, b) -> [] (*compile_i_binop op a b *)
  | Icall  (label, body) -> []


let compile_main name nb_var body=
  Label(name)::save_fp@allocate_mem nb_var@compile_i_ast body@ restore_fp nb_var @ [End_of_program]

let compile_fun name nb_var body =
  start_of_fun name nb_var@compile_i_ast body@end_of_fun nb_var

(* Compile le programme p et enregistre le code dans le fichier ofile *)
let to_mips (p,data) ofile =
  let aux (name, nb_var, body) = match name with
  | "main" -> compile_main name nb_var body
  | name ->  compile_fun name nb_var body in
  let code = List.map aux p |> List.concat in
  let p = { text = code; data = [] } in
  Mips.print_program p ofile
