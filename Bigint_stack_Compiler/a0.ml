type bigint = sign * (int list) and sign = Neg | NonNeg;;

	let head = List.hd;;
	let tail = List.tl;;
	let length = List.length;;
	let reverse = List.rev;;
	let radix = 10;;

	type tupleListInt = int * int;;

	exception NotFound;;
	exception Empty_list;;
	exception BadLanguage;;
	exception CannotDivideByZero;;

(*--------------------------IMP-HELPER-FUNCTIONS-----------------------------*)
	let rec fillList l1 l2 = match l1,l2 with
		|l1,l2 ->
			if (length l1)<(length l2) then (fillList ([0]@l1) l2)
			else if (length l1)>(length l2) then (fillList l1 ([0]@l2))
			else l1,l2;;

	let extractFromTupleFirst s = match s with
		| (x,y) -> x;;

	(* extractFromTupleSecond((fillList [1;0;0;0] [1]));; => gives [0;0;0;1]*)
	let extractFromTupleSecond s = match s with
		| (x,y) -> y;;


	(*--Remove unnecessary zeros from left of string [Pretty neat, right]--*)
	let rec cleanList l = match l with
		| []->[]
		| x::xs ->
			if x<>0 then l
			else (cleanList xs);;

	(* Didn't know about string_of_int :/ *)		
	let intToString x = match x with
		| 1->"1"
		| 2->"2"
		| 3->"3"
		| 4->"4"
		| 5->"5"
		| 6->"6"
		| 7->"7"
		| 8->"8"
		| 9->"9"
		| 0->"0"
		| _-> raise NotFound;;

	let rec listString l1 = match l1 with
	| [] -> ""
	| x::xs -> (intToString x) ^ (listString xs);;
	(* | _ -> raise BadLanguage;; *)

	let checkSign n = match n with
	  	| x -> if x<0 then "Neg" else "NonNeg";;

	let rec intToList x = match x with
		| 0 -> [0]
		| x -> cleanList ((intToList (x/10)) @ [(x mod 10)]);; (* 4141 gives [0;4;1;4;1] *)

	let absInt x = if x<0 then (-1*x) else x;;

	let rec lengthList l1 = match l1 with
		| [] -> 0
		| x::xs -> 1 + (lengthList xs);;
(*------------------------------------------------------------------------*)


(*-----------------------COMPLEMENTARY-FNCTNS-----------------------------*)
	let rec cmp' list1 list2 =
	    if list1 = [] then
	      0
	    else if ((head list1) > (head list2))  then
	      1
	    else if ((head list1) < (head list2))  then
	      -1
	    else cmp' (tail list1) (tail list2);;

	let cmp value1 value2 = 
	    if ((value1 = []) || (value2 = []))  then
	      raise Empty_list
	    else if (length value1) > (length value2)  then
	      1
	    else if (length value1) < (length value2)  then
	      -1
	    else cmp' (reverse value1) (reverse value2);;

	(*Below two functions used for carrying operation in subtraction*)
	let modifySub l1 = match l1 with
		|[]->[]
		|x::xs -> (x-1)::xs;;

	let modifymodifySub l1 = match l1 with
		|[] -> []
		|x::xs -> match xs with	
					| [] -> []
					| xs1::xs2 -> [9]@[xs1-1]@xs2;;
(*--------------------------------------------------------------*)

	(* let sumList l1 l2 = 
	  	let rec loop carry result l1 l2 = 
	  		match l1, l2, carry with
			| [], [], c -> [c]@result
			| [], x::xs, _ ->
			  let sumList = (x+carry) in
		  	  loop carry (( sumList mod 10 )::result) [] xs
			| x::xs, [], _ ->
			  let sumList = (x + carry) in
		  	  loop carry ((sumList mod 10)::result) [] xs
			| x1::xs1, x2::xs2, _ ->
		  	  let sumList = x1+x2+carry in
		  	  let carry = (sumList / 10) in
		  	  loop carry ((sumList mod 10) :: result) xs1 xs2
	    in
	  	loop 0 [] l1 l2;; *)


	(*--| add': add two lists | The operations are on inverted lists |-------*)
	let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix);;

    (*--| subList: subtract two lists (where l1 > l2) | The operations are on inverted lists |-------*)
	let rec subList l1 l2 carry = match l1, l2, carry with
		| [],[],0 -> [0]
		| l1,[],0 -> l1
		| [], l2, 0 -> l2 (* should not come to this *)
		| l1, [], carry -> subList l1 [carry] 0 (*should not come to this*)
		| [], l2, carry -> subList [carry] l2 0 
		| x1::xs1, x2::xs2, carry -> 
			let diff = x1-x2+carry
			in 	if diff<0 then 
					if (head xs1) <> 0 then (x1-x2+carry+10)::subList (modifySub xs1) xs2 0
					else  (x1-x2+carry+10)::subList (modifymodifySub xs1) xs2 0
			   	else  (x1-x2+carry)::subList xs1 xs2 0;;


	(*|True sub and add functions over lists|*)
	let addCallList l1 l2 = cleanList (reverse (add' (List.rev l1) (List.rev l2) 0));;
	(* let subCallList l1 l2 = reverse (subList (reverse l1) (reverse l2) 0) *)
	let subCallList l1 l2 = 
		if(cmp l1 l2)<>0 then cleanList (reverse (subList (reverse (extractFromTupleFirst (fillList l1 l2))) (reverse (extractFromTupleSecond (fillList l1 l2))) 0))
		else [0];;

	let decrementList l1 = 
		if l1=[1] then [0]
		else subCallList l1 [1];;


	(*-------------------TRUE FUNCTION CALLING-----------------------*)
	let add b1 b2 = match b1, b2 with
		| (NonNeg,l1), (NonNeg, l2) -> (NonNeg,(addCallList l1 l2))
		| (Neg, l1), (Neg, l2) -> (Neg, (addCallList l1 l2))
		| (NonNeg, l1), (Neg, l2) -> 
			let diff = (cmp l1 l2) in
				if diff=1 then (NonNeg,(subCallList l1 l2))
				else (Neg, (subCallList l2 l1))
		| (Neg,l1), (NonNeg,l2) ->
			let diff = (cmp l1 l2) in
				if diff<1 then (NonNeg,(subCallList l2 l1))
				else (Neg, (subCallList l1 l2));;

	let sub b1 b2 = match b1,b2 with
		| (NonNeg,l1), (NonNeg, l2) ->(* (add (P,l1) (Neg,l2))*)
			if (cmp l1 l2)=1 then (NonNeg,(subCallList l1 l2))
		    else if (cmp l1 l2)=0 then (NonNeg, (subCallList l1 l2)) 
		    else (Neg,(subCallList l2 l1))
		| (Neg, l1), (NonNeg, l2) -> (Neg, (addCallList l1 l2)) 
		| (NonNeg, l1), (Neg, l2) -> (add (NonNeg,l1) (NonNeg,l2))
		| (Neg,l1), (Neg,l2) -> 
			if (cmp l2 l1)=1 then (NonNeg,(subCallList l2 l1))
		    else if (cmp l1 l2)=0 then (NonNeg, (subCallList l1 l2)) 
			else (Neg, (subCallList l1 l2));;

	(* let rec multList l1 l2 ans = match l1,l2,ans with
		|[1],l2,ans -> addCallList l2 ans
		|l1,[1],ans -> addCallList l1 ans
		|l1,l2,ans -> multList (subCallList l1 [1]) l2 (addCallList l2 ans);; *)
(*-----------------------------dwegsg-------------------------------*)
	let rec map f l = match l with
		|[]->[]
		|x::xs -> (f x)::(map f xs);;

	 let concat_list list1 = 
        int_of_string (String.concat "" 
            (map string_of_int list1));;



	let double number = (addCallList number number);;

	(* INSTRUCTION: mulTuple [1;2;3;4] [1] [1;2;3;5] *)
	let rec  mulTuple list1 power list2 = 
        if concat_list power > concat_list list1
        then list1, [0]
        else let remainder, product =
            mulTuple list1 (double power) (double list2)
        in if concat_list remainder < concat_list power
            then remainder, product
            else (subCallList remainder power), (addCallList product list2);;

    let multList l1 l2 = extractFromTupleSecond (mulTuple l1 [1] l2);;

	let mult b1 b2 = match b1,b2 with
		|(NonNeg,l1),(NonNeg,l2) -> (NonNeg,(multList l1 l2 ))
		|(NonNeg,l1),(Neg,l2) -> (Neg,(multList l1 l2))
		|(Neg,l1),(Neg,l2) -> (NonNeg,(multList l1 l2))
		|(Neg,l1),(NonNeg,l2) -> (Neg,(multList l1 l2));;
		(* |_ -> raise BadLanguage;; *)

	(* This instr is helper for division in list *)
	let rec rem' list1 power list2 =
        if concat_list list2 > concat_list list1
        then [0], list1
        else let quotient, remainder =
            rem' list1 (double power) (double list2) in
            if concat_list remainder < concat_list list2
            then quotient, remainder
            else (addCallList quotient power), (subCallList remainder list2);;

    (* Below two functions are for dividing lists *)
    let remList l1 l2 = extractFromTupleSecond(rem' l1 [1] l2);;
    let divList l1 l2 = extractFromTupleFirst(rem' l1 [1] l2);;

    (* This method was in earlier version, Not efficient *)
	(* let rec divList l1 l2 ans =
		if (cmp l1 l2)=(-1) then ans
		else if (cmp l1 l2)=0 then (addCallList ans [1])
	    else divList (subCallList l1 l2) l2 (addCallList ans [1]);; *)

	let div b1 b2 = match b1,b2 with 
		|(NonNeg,l1),(NonNeg,l2) -> (NonNeg,(divList l1 l2))
		|(NonNeg,l1),(Neg,l2) -> (Neg,(divList l1 l2))
		|(Neg,l1),(NonNeg,l2) -> (Neg,(divList l1 l2))
		|(Neg,l1),(Neg,l2) -> (NonNeg,(divList l1 l2));;
		(* |_ -> raise BadLanguage;;  I wish they allowed this*)

    let rem b1 b2 = sub b1 (mult b2 (div b1 b2));;

	let eq b1 b2 = match b1,b2 with
		| (NonNeg,l1),(NonNeg,l2) ->
			if (cmp l1 l2)=0 then true
			else false
		| (Neg,l1),(Neg,l2) ->
			if (cmp l1 l2)=0 then true
				else false
		| _,_ -> false;;

	let gt b1 b2 = match b1,b2 with
		| (NonNeg,_),(Neg,_) -> true
		| (Neg,_),(NonNeg,_) -> false
		| (NonNeg,l1),(NonNeg,l2) -> 
			if (cmp l1 l2)=1 then true
		    else false
		| (Neg,l1),(Neg,l2) -> 
			if (cmp l1 l2)=1 then false
			else true;;

	let lt b1 b2 = 
		if (gt b1 b2)=false && (eq b1 b2)=false then true
		else false;;

	let geq b1 b2 = 
		if (lt b1 b2)=false then true
		else false;;

	let leq b1 b2 = 
		if (gt b1 b2)=false then true
		else false;;

	let abs b = match b with
		|(NonNeg,l1) -> (NonNeg,l1)
		|(Neg,l1) -> (NonNeg,l1);;

	let minus b = match b with
		|(NonNeg,l1) -> (Neg,l1)
		|(Neg,l1) -> (NonNeg,l1);;	

	(* Bigint to string conversion*)
	let print_num b1 = match b1 with
		| (_,[])-> raise Empty_list
		| (Neg,l1) -> "-" ^ (listString l1)
		| (NonNeg,l1) -> (listString l1);;

	(* Integer to bigint conversion *)
	let mk_big n = match n with
		| x -> if checkSign (x) = "Neg" then (Neg, (cleanList (intToList (absInt x)))) else (NonNeg, (cleanList (intToList x)));;
