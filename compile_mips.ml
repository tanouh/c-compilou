(* Production de code pour notre langage *)

open Mips
open Ast

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

let compile_print  = ()


let rec compile_i_ast = function
  | Iif (e, a, b) -> []
  | Iblock a -> []
| Ireturn e -> []
  | Iassign (l,e) -> []
| Ival e -> []

and compile_i_left_value = function
| Ileft l -> []
| Iconst k -> []

and compile_i_pos = function
| Ilocal offset (* offset to FP *) -> []
| Iglobal label (* label *) -> []
| Ideref p (* for pointers *) -> []
and compile_i_expr = function
| Iunop v -> []
| Ibinop (op, a, b) ->  []
| Icall  (pos, label, offset) -> []




let compile_stmt programlist = ()



(* Compile le programme p et enregistre le code dans le fichier ofile *)
let compile_program p ofile =
  let code = List.map compile_stmt p |> List.concat in
  let p = { text = code; data = List.of_seq (Hashtbl.to_seq_values gvars) } in
  Mips.print_program p ofile
