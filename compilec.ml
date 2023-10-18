open Ast
open Mips

module StrMap = Map.Make (String)

exception Error of string

let make_ast () = 5

let to_mips program = program

let compile_stmt p = []

(* Compile le programme p et enregistre le code dans le fichier ofile *)
let compile_program p ofile =
  let code = List.map compile_stmt p |> List.concat in
  let p = to_mips { text = code; data = [] } in
  Mips.print_program p ofile
