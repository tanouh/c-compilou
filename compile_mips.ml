(* Production de code pour notre langage *)

open Mips
open Ast
module StrMap = Map.Make (String)

let compile_stmt p = []



(* Compile le programme p et enregistre le code dans le fichier ofile *)
let compile_program p ofile =
  let code = List.map compile_stmt p |> List.concat in
  let p = { text = code; data = List.of_seq (Hashtbl.to_seq_values gvars) } in
  Mips.print_program p ofile
