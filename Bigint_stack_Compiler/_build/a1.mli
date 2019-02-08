(*  Assignment 1: A simple definitional interpreter and stack machine
In this assignment, you will model the "abstract syntax" of a simple calculator language for integer expressions, and give it "semantic meaning" in terms of OCaml's built-in types, and in the second part, implement the calculator as a simple stack-based machine, for which we have opcodes into which we compile the abstract syntax of an expression.
*)

open A0
   (* abstract syntax  *)
  type  exptree =  N of bigint
    | Plus of exptree * exptree
    | Minus of exptree * exptree
    | Mult of exptree * exptree
    | Div of exptree * exptree
    | Rem of exptree * exptree
    | Nega of exptree (* Neg is for sign in BigInt. Nega is negative of expression  *)
    | Abs of exptree

  exception BadFormat
  (* opcodes of the stack machine *)
  type opcode = CONST of bigint | PLUS | TIMES | MINUS | DIV | REM | ABS | UNARYMINUS

  (* the definitional interpreter *)
  val eval : exptree -> bigint

  (* For simplicity the stack will be implemented as a list of bigint values, and the program is a list of opcodes.  If you have not implemented the bigint package, you may use int for reduced credits. The stack machine is to be defined as a tail-recursive function *)
  val stackmc: (bigint list) -> (opcode list) -> bigint

  (* val compile: exptree -> opcode list *)
  val compile: exptree -> opcode list
