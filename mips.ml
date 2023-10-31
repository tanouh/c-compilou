type register =
  | ZERO
  | A of int
  | V of int
  | RA
  | SP
  | GP
  | FP
  | T of int
  | S of int
  | HI
  | LO

type address = Alab of string | Areg of int * register
type arith = Add | Sub | Mul | Div

type instruction =
  | Move of register * register
  | Li of register * int
  | Lw of register * address
  | Sw of register * address
  | Arith of arith * register * register * register
  | Arithi of arith * register * register * int
  | Arith_div of register * register
  | Slt of register * register * register
  | Sltu of register * register * int
  | Xori of register * register * int
  | Xor of register * register * register
  | And of register * register * register
  | Or of register * register * register
  | Jal of string
  | J of string
  | Jr of register
  | Mfhi of register
  | Mflo of register
  | Syscall
  | Label of string
  | Comment of string
  | Endfun of string
  | JEnd of string
  | End_of_program
  | Bgtz of register * string
  | Beq of register * register * string

type data = Asciiz of string * string | Word of string * int
type program = { text : instruction list; data : data list }

open Format

let string_register = function
  | ZERO -> "$zero"
  | A i -> "$a" ^ string_of_int i
  | V i -> "$v" ^ string_of_int i
  | RA -> "$ra"
  | SP -> "$sp"
  | FP -> "$fp"
  | GP -> "$gp"
  | T i -> "$t" ^ string_of_int i
  | S i -> "$t" ^ string_of_int i
  | HI -> "$hi"
  | LO -> "$lo"

let string_arith = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"

let string_address = function
  | Alab s -> s (* Adress label *)
  | Areg (ofs, r) -> string_of_int ofs ^ "(" ^ string_register r ^ ")"

let jlabel k = "$Compilou" ^ string_of_int k

let string_instruction = function
  | Move (dst, src) ->
      "\tmove\t" ^ string_register dst ^ ", " ^ string_register src
  | Li (r, i) -> "\tli\t" ^ string_register r ^ ", " ^ string_of_int i
  | Lw (r, a) -> "\tlw\t" ^ string_register r ^ "," ^ string_address a
  | Sw (r, a) -> "\tsw\t" ^ string_register r ^ "," ^ string_address a
  | Arith (op, dst, src, src2) ->
      "\t" ^ string_arith op ^ "\t" ^ string_register dst ^ ","
      ^ string_register src ^ "," ^ string_register src2
  | Arithi (op, dst, src, src2) ->
      "\t" ^ string_arith op ^ "\t" ^ string_register dst ^ ","
      ^ string_register src ^ "," ^ string_of_int src2
  | Arith_div (src, src2) ->
      "\tdiv\t" ^ string_register src ^ "," ^ string_register src2
  | Slt (dst, src, src2) ->
      "\tslt\t" ^ string_register dst ^ "," ^ string_register src ^ ","
      ^ string_register src2
  | Sltu (dst, src, src2) ->
      "\tsltu\t" ^ string_register dst ^ "," ^ string_register src ^ ","
      ^ string_of_int src2
  | Xori (dst, src, src2) ->
      "\txori\t" ^ string_register dst ^ "," ^ string_register src ^ ","
      ^ string_of_int src2
  | Xor (dst, src, src2) ->
      "\txor\t" ^ string_register dst ^ "," ^ string_register src ^ ","
      ^ string_register src2
  | And (dst, src, src2) ->
      "\tand\t" ^ string_register dst ^ "," ^ string_register src ^ ","
      ^ string_register src2
  | Or (dst, src, src2) ->
      "\tor\t" ^ string_register dst ^ "," ^ string_register src ^ ","
      ^ string_register src2
  | Mfhi dst -> "\tmfhi\t" ^ string_register dst
  | Mflo dst -> "\tmflo\t" ^ string_register dst
  | Jal s -> "\tjal\t" ^ s
  | J s -> "\tj\t" ^ s
  | Jr r -> "\tjr\t" ^ string_register r
  | Syscall -> "\tsyscall"
  | Comment s -> "\t " ^ s
  | Label s -> s ^ ":"
  | Endfun s -> "end_" ^ s ^ ":"
  | JEnd s -> "\tj\tend_" ^ s
  | End_of_program -> "\tli $v0, 10\n\tsyscall"
  | Bgtz (r, label) -> "\tbgtz\t" ^ string_register r ^ "," ^ label
  | Beq (r1, r2, label) ->
      "\tbeq\t" ^ string_register r1 ^ "," ^ string_register r2 ^ "," ^ label

let string_data = function
  | Asciiz (l, s) -> l ^ ":\t.asciiz '" ^ String.escaped s ^ "'"
  | Word (l, n) -> l ^ ": \t.word " ^ string_of_int n

let print_program p out_filename =
  let out_file = open_out out_filename in
  let add s = Printf.fprintf out_file "%s\n" s in
  add ".data";
  List.iter (fun e -> string_data e |> add) p.data;
  add ".text";
  List.iter (fun e -> string_instruction e |> add) p.text;
  close_out out_file
