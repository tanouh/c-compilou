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

val jlabel : int -> string
val print_program : program -> string -> unit
