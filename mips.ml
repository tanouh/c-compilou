type register =
  | ZERO | A0 | A1 | V0  | RA | SP | GP | FP | T of int | S of int | HI | LO

type address =
  | Alab of string
  | Areg of int * register

type arith  = Add | Sub | Mul | Div | Mod | Leq | Le | Geq | Ge | Neq | Eq | And | Or

type instruction =
  | Move of register * register
  | Li of register * int
  | Lw of register * address
  | Sw of register * address
  | Arith of arith * register * register * register
  | Arithi of arith * register * register * int
  | Jal of string
  | J of string
  | Jr of register
  | Syscall
  | Label of string
  | Comment of string
  | Endfun of string
  | JEnd of string
  | End_of_program

type data =
  | Asciiz of string * string
  | Word of string * int
type program = {
  text : instruction list;
  data : data list;
}

open Format

let string_register = function
  | ZERO -> "$zero"
  | A0 -> "$a0"
  | A1 -> "$a1"
  | V0 -> "$v0"
  | RA -> "$ra"
  | SP -> "$sp"
  | FP -> "$fp"
  | GP -> "$gp"
  | T i -> "$t"^(string_of_int i )
  | S i -> "$t"^(string_of_int i )
  | HI ->"$hi"
  | LO -> "$lo"

let string_arith = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Mod -> "mult"
  | Leq -> ""
  | Le -> ""
  | Geq -> ""
  | Ge -> ""
  | Neq -> ""
  | Eq -> ""
  | And -> "and"
  | Or-> "or"


let string_address = function
  | Alab s ->  s (* Adress label *)
  | Areg (ofs, r) -> (string_of_int ofs)^"("^(string_register r)^")"


let string_instruction = function
  | Move (dst, src) ->
      "\tmove\t"^(string_register dst)^", "^(string_register src)
  | Li (r, i) ->
     "\tli\t"^(string_register r)^", "^(string_of_int i)
  | Lw (r, a) ->
    "\tlw\t"^(string_register r)^","^(string_address a)
  | Sw (r, a) ->
     "\tsw\t"^(string_register r)^","^(string_address a)
  | Arith (op, dst, src, src2) ->
     "\t"^(string_arith op)^"\t"^(string_register dst)^","^(string_register src)^","^(string_register src2)
  | Arithi (op, dst, src, src2) ->
     "\t"^(string_arith op)^"\t"^(string_register dst)^","^(string_register src)^","^(string_of_int src2)
  | Jal s -> "\tjal\t"^s
  | J s -> "\tj\t"^s
  | Jr r -> "\tjr\t"^(string_register r)
  | Syscall -> "\tsyscall"
  | Comment s ->  "\t "^s
  | Label s ->  s^":"
  | Endfun s -> "end_"^s^":"
  | JEnd s -> "\tj\tend_"^s
  | End_of_program -> "\tli $v0, 10\n\tsyscall"

let string_data = function
  | Asciiz (l, s) ->
      l^":\t.asciiz '"^(String.escaped s)^"'"
  | Word (l, n) ->
     l^": \t.word "^(string_of_int n)
let print_program p out_filename =
  let out_file = open_out out_filename in
  let add s =
     Printf.fprintf out_file "%s\n" s;
  in
  add ".data";
  List.iter (fun e -> string_data e |> add ) p.data ;
  add ".text";
  List.iter (fun e -> string_instruction e |> add ) p.text  ;
  close_out out_file

