
type register =
  | ZERO | A0 | A1 | V0  | RA | SP | GP | FP | T of int | S of int | HI | LO


type address =
  | Alab of string
  | Areg of int * register

type arith = Add | Sub | Mul | Div | Mod | Leq | Le | Geq | Ge | Neq | Eq | And | Or

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

val print_program : program -> string -> unit
