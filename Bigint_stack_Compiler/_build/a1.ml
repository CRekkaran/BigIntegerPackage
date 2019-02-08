open A0

	type  exptree =  N of bigint
    	| Plus of exptree * exptree
    	| Minus of exptree * exptree
    	| Mult of exptree * exptree
    	| Div of exptree * exptree
    	| Rem of exptree * exptree
    	| Nega of exptree (* Neg is for sign in BigInt. Nega is negative of expression  *)
    	| Abs of exptree;;

    let head = List.hd
    exception BadFormat

    let rec reversemi l l1 = match l with
    	|[]->l1
    	|x::xs -> reversemi xs (x::l1)

    let rec appendmi l1 l2 = match l1 with
    	|[]->l2
    	|x1::xs1 -> x1::(appendmi xs1 l2)

    (* INSTRUCTION: eval Plus (b1,Mult (b2,b3)) *)
    let rec eval e = match e with
    	|N(i) -> i
    	|Plus(e1,e2) -> add (eval e1) (eval e2)
    	|Mult(e1,e2) -> mult (eval e1) (eval e2)
    	|Minus(e1,e2) -> sub (eval e1) (eval e2)
    	|Div(e1,e2) -> div (eval e1) (eval e2)
    	|Rem(e1,e2) -> rem (eval e1) (eval e2)
    	|Nega(e1) -> mult (Neg,[1]) (eval e1)
    	|Abs(e1) -> abs (eval e1)
    		(* if (eval (Nega e1))>0 then eval (Nega e1)
    	    else (eval e1);; *)


    type opcode = CONST of bigint | PLUS | TIMES | MINUS | DIV | REM | ABS | UNARYMINUS 

    (* INSTRUCTION : compiler (Plus (N (NonNeg,[4;1;2])),(N (Neg,[5;3;5;1;2]))) [] *)
    let rec compiler t l = match t with 
    	|N(i) -> (CONST i)::l
    	(* |Plus(t1,t2) -> reversemi (PLUS::(reversemi (appendmi (compiler t1 []) (compiler t2 []))));; *)
    	|Plus(t1,t2) -> appendmi (appendmi (compiler t1 []) (compiler t2 [])) [PLUS]
    	|Mult(t1,t2) -> appendmi (appendmi (compiler t1 []) (compiler t2 [])) [TIMES]
    	|Minus(t1,t2) -> appendmi (appendmi (compiler t1 []) (compiler t2 [])) [MINUS]
    	|Div(t1,t2) -> appendmi (appendmi (compiler t1 []) (compiler t2 [])) [DIV]
    	|Rem(t1,t2) -> appendmi (appendmi (compiler t1 []) (compiler t2 [])) [REM]
    	|Nega(t1) -> appendmi (compiler t1 []) [UNARYMINUS]
    	|Abs(t1) -> appendmi (compiler t1 []) [ABS]

    let compile t = compiler t []


    (* INSTRUCTION: stackmc [] [opcode list] ------- example: stackmc [] (compiler (Plus (N b1),(N b2))) *)
    let rec stackmc s l = match l with
    	|[]-> (head s)
    	|CONST(i)::l'-> (stackmc (i::s) l')
    	|PLUS::l' -> (match s with
    					|b1::(b2::s') -> stackmc ((add b1 b2)::s') l'
    					|_ -> raise BadFormat)
    	|MINUS::l' -> (match s with
    					|b1::(b2::s') -> stackmc ((sub b1 b2)::s') l'
    					|_ -> raise BadFormat)
    	|TIMES::l' -> (match s with
    					|b1::(b2::s') -> stackmc ((mult b1 b2)::s') l'
    					|_-> raise BadFormat)
    	|UNARYMINUS::l' -> (match s with
    						|b1::s' -> stackmc ((minus b1)::s') l'
    						|_ -> raise BadFormat)
    	|ABS::l' -> (match s with 
    					|b1::s' -> stackmc ((abs b1)::s') l'
    					|_ -> raise BadFormat)
    	|DIV::l' -> (match s with 
    					|b1::(b2::s') -> stackmc ((div b1 b2)::s') l'
    					|_ -> raise BadFormat)
    	|REM::l' -> (match s with 
    					|b1::(b2::s') -> stackmc ((rem b1 b2)::s') l'
    					|_ -> raise BadFormat)