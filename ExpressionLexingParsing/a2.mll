{
  open A3
  exception Not_implemented
  exception Invalid_Input
  exception UselessZeroes
}

(*
  Below is a dummy implementation. Please note the following
  - Tokens are defined in A3.mly
  - Return type is token and not token list
  - End of buffer is indicated by EOF token below
  - There is no trailer. The scanner function is written in the wrapper file (test_a3.ml)
*)

let digit = ['0'-'9']
let alpha = ['a'-'z']
let alpha_caps = ['A'-'Z']
let white_space = (" " | '\t')

rule read = parse
   |eof                                                    	{ EOF }
   |white_space+											                      {read lexbuf}	
   | 'T'                                                    { (BOOL (bool_of_string "true")) }
   | 'F'                                                    { (BOOL (bool_of_string "false")) }  
   | ['0']+['0'-'9']+                                       {raise UselessZeroes}
   | ['0'-'9']+ as n                                      	{ INT (int_of_string n) }
   | ['A'-'Z'](['a'-'z' 'A'-'Z']*['0'-'9']*[''']*['_']*)* as s        	{ (ID s) }
   
   | "abs"              							 		                  { ABS }
   |"mod"                                                   {REM} 
   | '~' 													                          {TILDA}
   | "not"|'!' 													                    {NOT}
   | '+' 													                          {PLUS}
   | '-' 													                          {MINUS}
   | '*' 													                          {TIMES}
   | "div" 													                        {DIV}
   | '%' 													                          {REM}
   | "/\\" 													                        {CONJ}
   | "\\/" 													                        {DISJ}
   | '>' 													                          {GT}
   | '<' 													                          {LT}
   | '='                                                    {EQ}
   | "if" 												                          {IF}
   | "then" 											                          {THEN}
   | "else" 											                          {ELSE}
   | "fi" 												                          {FI}
   | ',' 													                          {COMMA}
   | "proj" 											                          {PROJ}
   | '('                           													{LP}
   | ')' 													                          {RP}
   | '\n' 													                        {EOL}
   | _                										                  { raise Invalid_Input }

{
  let scanner s = read (Lexing.from_string s)
}
