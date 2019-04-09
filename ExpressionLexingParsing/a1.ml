(* Dummy implementation of A1 *)
open A0
exception Not_implemented
exception Wrong_Syntax
exception Wrong_Type
exception Insufficient_Data

(* abstract syntax *)
type  exptree =  
  Var of string (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
  | N of int      (* Integer constant *)
  | B of bool     (* Boolean constant *)
  (* unary operators on integers *)
  | Abs of exptree                   (* abs *)
  | Negative of exptree              (* unary minus ~ *)
  (* unary operators on booleans *)
  | Not of exptree
  (* binary operators on integers *)
  | Add of exptree * exptree         (* Addition + *)
  | Sub of exptree * exptree         (* Subtraction - *)
  | Mult of exptree * exptree        (* Multiplication * *)
  | Div of exptree * exptree         (* div *)
  | Rem of exptree * exptree         (* mod *)
  (* binary operators on booleans *)
  | Conjunction of exptree * exptree (* conjunction /\ *)
  | Disjunction of exptree * exptree (* binary operators on booleans \/ *)
  (* comparison operations on integers *)
  | Equals of exptree * exptree      (* = *)
  | GreaterTE of exptree * exptree   (* >= *)
  | LessTE of exptree * exptree      (* <= *)
  | GreaterT of exptree * exptree    (* > *)
  | LessT of exptree * exptree       (* < *)
  (* expressions using parenthesis *)
  | InParen of exptree               (* ( ) *)
  (* a conditional expression *)
  | IfThenElse of exptree * exptree * exptree (* if then else fi  *)
  (* creating n-tuples (n >= 0) *)
  | Tuple of int * (exptree list)
  (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Project of (int*int) * exptree   (* Proj((i,n), e)  0 < i <= n *)

(* opcodes of the stack machine (in the same sequence as above) *)
type opcode = VAR of string | NCONST of bigint | BCONST of bool | ABS | UNARYMINUS | NOT
  | PLUS | MINUS | MULT | DIV | REM | CONJ | DISJ | EQS | GTE | LTE | GT | LT
  | PAREN | IFTE | TUPLE of int | PROJ of int*int

type value = NumVal of int | BoolVal of bool | TupVal of int * (value list)

(* The language should contain the following types of expressions:  integers and booleans *)
type answer = Num of bigint | Bool of bool | Tup of int * (answer list)

let rec map f l rho = match l with
	|[] -> []
	|x::xs -> (f x rho)::(map f xs rho)

let giveOneFromTwo (a,b) i = if i=1 then a else if i=2 then b else raise Wrong_Syntax

let rec eval ex rho = match ex with
	|(Var x) -> (rho x)
	|(N x) -> (NumVal x)
	|(B x) -> (BoolVal x)
	|(Abs x) -> (match (eval x rho) with 
		|(NumVal c) -> if c<0 then (NumVal (-1*c)) else (NumVal c)
		|_ -> raise Wrong_Type
	)
	(* x is now bigInt *)
	|(Negative x) -> eval (Mult (x,(N (-1)))) rho(*Num (mult (eval x rho) (mk_big (-1)))*)
	|(Not x) -> if (eval x rho)=(BoolVal true) then (BoolVal false) else (BoolVal true)
	|(Add (e1,e2)) -> (match ((eval e1 rho),(eval e2 rho)) with
		|(NumVal x1),(NumVal x2) -> NumVal (x1+x2)
		|_ -> raise Wrong_Type)
	|(Sub (e1,e2)) -> (match ((eval e1 rho),(eval e2 rho)) with
		|(NumVal x1),(NumVal x2) -> NumVal (x1 - x2)
		|_ -> raise Wrong_Type)
	|(Mult (e1,e2)) -> (match ((eval e1 rho),(eval e2 rho)) with
		|(NumVal x1),(NumVal x2) -> NumVal (x1*x2)
		|_ -> raise Wrong_Type)
	|(Div (e1,e2)) -> (match ((eval e1 rho),(eval e2 rho)) with
		|(NumVal x1),(NumVal x2) -> NumVal (x1/x2)
		|_ -> raise Wrong_Type)
	|(Rem (e1,e2)) -> (match (eval e1 rho),(eval e2 rho) with
		|(NumVal x1),(NumVal x2) -> NumVal (x1 mod x2)
		|_ -> raise Wrong_Type)
	|(Conjunction (x1,x2)) -> (match ((eval x1 rho),(eval x2 rho)) with
		|BoolVal (true),BoolVal(true) -> BoolVal(true)
		|BoolVal(true),BoolVal(false) -> BoolVal(false)
		|BoolVal(false),BoolVal(true) -> BoolVal(false)
		|BoolVal(false),BoolVal(false) -> BoolVal(false)
		|_ -> raise Not_implemented)
	|(Disjunction (x1,x2)) -> (match ((eval x1 rho),(eval x2 rho)) with
		|BoolVal(true),BoolVal(true) -> BoolVal(true)
		|BoolVal(true),BoolVal(false) -> BoolVal(true)
		|BoolVal(false),BoolVal(true) -> BoolVal(true)
		|BoolVal(false),BoolVal(false) -> BoolVal(false)
		|_ -> raise Wrong_Type)
	|(Equals (x1,x2)) -> (match ((eval x1 rho), (eval x2 rho)) with
		|((NumVal a1),(NumVal a2)) -> if (a1=a2)=true then BoolVal(true) else BoolVal(false)
		|_ -> raise Wrong_Type)
	|(GreaterTE (e1,e2)) -> (match ((eval e1 rho),(eval e2 rho)) with
		|((NumVal a1),(NumVal a2)) -> if (a1>=a2)=true then (BoolVal true) else BoolVal(false)
		|_ -> raise Wrong_Type)
	|(LessTE (x1,x2)) -> (match ((eval x1 rho), (eval x2 rho)) with
		|(NumVal a1),(NumVal a2) -> if (a1<=a2)=true then (BoolVal true) else (BoolVal false)
		|_ -> raise Wrong_Type)
	|(LessT (x1,x2)) -> (match ((eval x1 rho), (eval x2 rho)) with
		|(NumVal a1),(NumVal a2) -> if (a1<a2)=true then (BoolVal true) else (BoolVal false)
		|_ -> raise Wrong_Type)
	|(GreaterT (x1,x2)) -> (match ((eval x1 rho), (eval x2 rho)) with
		|(NumVal a1),(NumVal a2) -> if (a1>a2)=true then (BoolVal true) else (BoolVal false)
		|_ -> raise Wrong_Type)
	|(InParen x1) -> (eval x1 rho)
	|(IfThenElse (x1,x2,x3)) -> if (eval x1 rho) = (BoolVal true) then (eval x2 rho) else (eval x3 rho)
	|(Tuple(x,list1)) -> (match (x,list1) with
		|(a,x::xs) -> TupVal(a, (map eval (x::xs) rho))
		|_ -> raise Wrong_Type)
	|(Project((a,b),x1)) -> if a<=b then (match (eval x1 rho) with
									|TupVal(d,(k::ks)) -> if d=b then (List.nth (k::ks) (a-1)) else raise Wrong_Syntax
									|_ -> raise Wrong_Type) else raise Wrong_Syntax



let absoBool b = match b with
	|Bool(true) -> Bool(true)
	|Bool(false) -> Bool(true)
	|_ -> raise Wrong_Syntax

let conjunctionBool t1 t2 = match t1,t2 with 
	|(Bool true),(Bool false) -> (Bool false)
	|(Bool true),(Bool true) -> (Bool true)
	|(Bool false),(Bool false) -> (Bool false)
	|(Bool false),(Bool true) -> (Bool false)
	|_ -> raise Wrong_Syntax

let disjunctionBool t1 t2 = match t1,t2 with
	|(Bool true),(Bool false) -> (Bool true)
	|(Bool true),(Bool true) -> (Bool true)
	|(Bool false),(Bool false) -> (Bool false)
	|(Bool false),(Bool true) -> (Bool true)
	|_ -> raise Wrong_Syntax		


let rec extract n ll = match (n,ll) with
	|(0,_) -> ([],ll)
	|(a,x::xs) -> ((x::(giveOneFromTwo (extract (a-1) xs) 1)  ),(giveOneFromTwo (extract (a-1) xs) 2))
	|(_,_) -> raise Wrong_Syntax 

let rho s = match s with 
   "X" -> Num (NonNeg,[5])
   |_ -> raise Wrong_Syntax
(*b1: Num bigint*)
let rec stackmc stk rho pgm = match pgm with
	|[] -> (List.hd stk)
	|(VAR x)::l' -> (stackmc ((rho x)::stk) rho l')
	|(NCONST x)::l' -> (stackmc ((Num x)::stk) rho l')
	|(BCONST x)::l' -> (stackmc ((Bool x)::stk) rho l')
	|ABS::l' -> (match stk with
					|(b1::s') -> (match b1 with	
									|Num (ass,x::xs) -> let d = (abs (ass,x::xs)) in (stackmc ((Num d)::s') rho l')
									|_ -> raise Not_implemented)
					|_ -> raise Wrong_Syntax
				)
	|UNARYMINUS::l' -> (match stk with
							|(b1::s') -> (match b1 with
								|Num (ass,x::xs) -> let d = (minus (ass,x::xs)) in (stackmc ((Num d)::s') rho l')
								|_ -> raise Wrong_Type)
							|_ -> raise Wrong_Syntax
						)
	|NOT::l' -> (match stk with
					|(b1::s') -> (match b1 with
											|(Bool true) -> (stackmc ((Bool false)::s') rho l')
											|(Bool false) -> (stackmc ((Bool true)::s') rho l')
											|_ -> raise Wrong_Type)
					|_ -> raise Wrong_Syntax
				)
	|PLUS::l' -> (match stk with
					|((Num b1)::((Num b2)::s')) -> stackmc ((Num (add b1 b2))::s') rho l'
					|_ -> raise Insufficient_Data
				)
	|MINUS::l' -> (match stk with
					|((Num b1)::((Num b2)::s')) -> stackmc ((Num (sub b1 b2))::s') rho l'
					|_ -> raise Insufficient_Data
				)
	|MULT::l' -> (match stk with
					|((Num b1)::((Num b2)::s')) -> stackmc ((Num (mult b1 b2))::s') rho l'
					|_ -> raise Insufficient_Data
				)
	|DIV::l' -> (match stk with
					|((Num b1)::((Num b2)::s')) -> stackmc ((Num (div b1 b2))::s') rho l'
					|_ -> raise Insufficient_Data
				)
	|REM::l' -> (match stk with
					|((Num b1)::((Num b2)::s')) -> stackmc ((Num (rem b1 b2))::s') rho l'
					|_ -> raise Insufficient_Data
				)
	|CONJ::l' -> (match stk with
					|(t1::(t2::s')) -> stackmc ((conjunctionBool t1 t2)::s') rho l'
					|_ -> raise Insufficient_Data
				)
	|DISJ::l' -> (match stk with
					|(t1::(t2::s')) -> stackmc ((disjunctionBool t1 t2)::s') rho l'
					|_ -> raise Insufficient_Data
				)
	|EQS::l' -> (match stk with
					|((Num b1)::((Num b2)::s')) -> stackmc ((Bool (eq b1 b2))::s') rho l'
					|_ -> raise Insufficient_Data
				)
	|GTE::l' -> (match stk with
					|((Num b1)::((Num b2)::s')) -> stackmc ((Bool (geq b1 b2))::s') rho l'
					|_ -> raise Insufficient_Data
				)
	|GT::l' -> (match stk with
					|((Num b1)::((Num b2)::s')) -> stackmc ((Bool (gt b1 b2))::s') rho l'
					|_ -> raise Insufficient_Data
				)
	|LT::l' -> (match stk with
					|((Num b1)::((Num b2)::s')) -> stackmc ((Bool (lt b1 b2))::s') rho l'
					|_ -> raise Insufficient_Data
				)
	|LTE::l' -> (match stk with
					|((Num b1)::((Num b2)::s')) -> stackmc ((Bool (leq b1 b2))::s') rho l'
					|_ -> raise Insufficient_Data
				)
	|PAREN::l' -> (stackmc stk rho l')
	|IFTE::l' -> (match stk with
					|((Bool dfg)::((dd)::((d)::s'))) -> if (dfg==true) then (stackmc (dd::s') rho l') else (stackmc (d::s') rho l')
					|_ -> raise Insufficient_Data 
				)	
	|(TUPLE x)::l' -> stackmc (   (Tup (x,(giveOneFromTwo (extract x stk) 1)))::(giveOneFromTwo (extract x stk) 2)   ) rho l'
	|(PROJ(a,b))::l' -> (match stk with
							|(Tup(n,x::xs)::xs1) -> if b=n && a<=b then (List.nth (x::xs) (a-1)) else raise Wrong_Syntax
							|_ -> raise Wrong_Syntax
						)



let rec compiler ex  l= match ex with
	|(Var x) -> (VAR x)::l
	|(N x) -> (NCONST (mk_big x))::l
	|(B x) -> (BCONST x)::l
	|(Abs x) -> (compiler x [])@[ABS]
	|(Negative x) -> (compiler x [])@[UNARYMINUS]
	|(Not x) -> (compiler x [])@[NOT]
	|(Add (x1,x2)) -> (compiler x2 [])@(compiler x1 [])@[PLUS]
	|(Sub (x1,x2)) -> (compiler x2 [])@(compiler x1 [])@[MINUS]
	|(Mult (x1,x2)) -> (compiler x2 [])@(compiler x1 [])@[MULT]
	|(Div (x1,x2)) -> (compiler x2 [])@(compiler x1 [])@[DIV]
	|(Rem (x1,x2)) -> (compiler x2 [])@(compiler x1 [])@[REM]
	|(Conjunction (x1,x2)) -> (compiler x2 [])@(compiler x1 [])@[CONJ]
	|(Disjunction (x1,x2)) -> (compiler x2 [])@(compiler x1 [])@[DISJ]
	|(Equals (x1,x2)) -> (compiler x2 [])@(compiler x1 [])@[EQS]
	|(GreaterTE (x1,x2)) -> (compiler x2 []) @(compiler x1 [])@[GTE]
	|(GreaterT (x1,x2)) -> (compiler x2 [])@(compiler x1 [])@[GT]
	|(LessT (x1,x2)) -> (compiler x2 [])@(compiler x1 [])@[LT]
	|(LessTE (x1,x2)) -> (compiler x2 [])@(compiler x1 [])@[LTE]
	|(InParen x) -> (compiler x [])@[PAREN]
	|(IfThenElse (x1, x2, x3)) -> (compiler x3 [])@(compiler x2 [])@(compiler x1 [])@[IFTE]
	|(Tuple(x,ll)) -> (let rec allreco l= match l with
							|[] -> []
							|x1::xs -> (allreco xs)@(compiler x1 [])
						in (allreco ll)@[TUPLE x])
	|(Project((a,b),x1)) -> (compiler x1 [])@[PROJ(a,b)]


let compile ex = compiler ex []
