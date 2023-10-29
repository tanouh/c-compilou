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

let allocate_mem nb_vars =
  [Arithi (Add, SP, SP, -4 * nb_vars)]

let free_mem nb_vars =
 [Arithi (Add, SP, SP, 4 * nb_vars)]

let save_fp = [Arithi (Add, SP, SP, -4); (Sw (FP, Areg(0,SP))); Move (FP, SP)]

let restore_fp nb_var = Lw (FP, Areg(0, FP))::free_mem (nb_var + 2)

 let end_of_fun nb_var=
  Lw (RA, Areg(-4, FP))::restore_fp nb_var@[Jr RA]

  let start_of_fun name nb_var =
    Label name::save_fp@allocate_mem(nb_var +1)@[Sw (RA, Areg(-4, FP))]


let rec compile_i_ast = function
  | Iif (e, a, b) -> []
  | Iblock a -> List.map compile_i_ast a |> List.concat
  | Ireturn e -> []
  | Iassign (l,e) -> compile_i_assign (l,e)
  | Ival e -> compile_i_expr e
  |No_op -> []
and compile_i_left_value (ipos,size) = match ipos with
  | Ilocal offset -> [Lw (V0, Areg (-size*offset, FP))]
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
    | Ilocal offset -> tmp @ [Sw (V0, Areg (-size*offset, FP))]
    | Iglobal label -> tmp @ [Sw (V0, Alab label)]
    |Ideref p -> tmp @  []
and compile_i_expr = function
  | Iconst k -> compile_i_const_value k
  | Ileft lv ->  compile_i_left_value lv
  | Iunop v -> compile_i_unop v
  | Ibinop (op, a, b) -> [] (*compile_i_binop op a b *)
  | Icall  (label, body) -> []



(* Compile le programme p et enregistre le code dans le fichier ofile *)
let to_mips (p,data) ofile =
  let aux (name, nb_var, body) = match name with
  | "main" -> Label(name)::save_fp@allocate_mem nb_var@compile_i_ast body @ restore_fp nb_var @ [End_of_program]
  | name -> start_of_fun name nb_var@compile_i_ast body@end_of_fun nb_var in
  let code = List.map aux p |> List.concat in
  let p = { text = code; data = [] } in
  Mips.print_program p ofile
