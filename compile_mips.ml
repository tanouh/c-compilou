(* Production de code pour notre langage *)

open Mips
open Ast_mips

module StrMap = Map.Make (String)

(* Les fonctions de manipulation de la pile pour rendre les instructions plus modulaires *)
let push_var reg locvars x =
  let l = [ Arithi (Add, SP, SP, -4); Sw (reg, Areg (4, SP)) ] in
  let locvars = StrMap.add x (StrMap.cardinal locvars + 1) locvars in
  l, locvars

(* Empiler une variable temporaire *)
let push_tmp =
  (* sp_aux := !sp_aux + 1; *)
  [ Arithi (Add, SP, SP, -4); Sw (V0, Areg (4, SP)) ]

(* Dépiler *)
let pop_tmp =
   (* sp_aux := !sp_aux - 1; *)
   [Arithi (Add, SP, SP, 4)]

let rec compile_i_ast = function
  | Iif (e, a, b) -> []
  | Iblock a -> []
  | Ireturn e -> []
  | Iassign (l,e) -> compile_i_assign (l,e)
  | Ival e -> compile_i_expr e
and compile_i_left_value (ipos,size) = match ipos with 
  | Ilocal offset -> [Lw (V0, Areg (size*offset, FP))]
  | Iglobal label -> [Lw (V0, Alab label)]
  |Ideref p -> []
and compile_i_const_value k = [Li (V0, k)]
and compile_i_unop v = 
  (compile_i_expr v) @ [Arithi (Mul, V0, V0, -1)] 
and compile_i_binop op a b = 
  match op with 
  | Add -> []
  | Sub -> []
  | Mul -> []
  | Div -> []
  | Mod -> []
  | Leq -> []
  | Le -> []
  | Geq -> []
  | Ge -> []
  | Neq -> []
  | Eq -> []
  | And -> []
  | Or -> []
and compile_i_assign (l,e) = 
  let tmp = compile_i_expr e and (ipos, size) = l in 
  match ipos with 
    | Ilocal offset -> tmp @ [Sw (V0, Areg (size*offset, FP))]
    | Iglobal label -> tmp @ [Sw (V0, Alab label)]
    |Ideref p -> tmp @  []
and compile_i_expr = function
  | Iconst k -> compile_i_const_value k
  | Ileft lv ->  compile_i_left_value lv
  | Iunop v -> compile_i_unop v
  | Ibinop (op, a, b) -> compile_i_binop op a b
  | Icall  (pos, label, offset, body) -> []



(* Compile le programme p et enregistre le code dans le fichier ofile *)
let compile_program (p,data) ofile =
  let code = List.map compile_i_ast p |> List.concat in
  let p = { text = code; data = List.of_seq (Hashtbl.to_seq_values gvars) } in
  Mips.print_program p ofile
